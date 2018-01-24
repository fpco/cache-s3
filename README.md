# cache-s3

This tool is designed to store files, that were produced during a CI build, to an S3 bucket, so that
they can be used by subsequent builds. Although this tool is tailored specifically for `stack`, it
is by no means limited to Haskell or `stack` users.

`cache-s3` is not simply a wrapper around a bunch of other tools, it is all written in Haskell,
which comes with a great benefit of being cross platform. Executable versions for common operating
systems can be downloaded from github [releases](https://github.com/fpco/cache-s3/releases) page.

## Problems it solves

CI providers already have some form of caching capability in place, so natural question comes to
mind is why do we even need to pay AWS for storage on S3, which we already get for free from Travis,
AppVeyor, CircleCI, etc. Here are the limitations with CI providers that addressed by `cache-s3`:

* __`stack` awareness__. None of the providers have support for
  [stack](https://docs.haskellstack.org/), which can be solved by complicated scripts that figure
  out which paths need caching and move the copious amounts files around so that they can be saved
  and restored properly.
* __Cache size limit__. Some providers limit the amount of data that can be retained between builds,
  while S3 is limited only by the cash in your pockets.
* __Cache sharing__. Most providers do not let you use cache from builds created by another branch.
* __Access to cache__. For providers like Travis, that do allow reading cache created for master, it
  can be read even by the forked repositories during the pull requests, which has a potential of
  leaking sensitive data. With `cache-s3` you have full access control by the means of S3 bucket IAM
  policies. Despite this, I would advise not to store any private data in the cache, there are better
  tools for managing sensitive information out there.

### Drawback

* Usage ain't free, gotta pay Amazon for S3.
* Saving and restoring cache will likely be slightly slower than CI provider's native solution,
  since data has to move over the Internet.


## Usage

There is an implicit assumption in this document that the user knows how to configure communication
with AWS from the command line (credentials, roles, accounts, regions, etc.), same as with `aws-cli`
for example. There are plenty of guides online how to get this setup.

### Prepare CI and S3

In order for the tool to work, an S3 bucket must already be setup on AWS. I would recommend setting
up a dedicated S3 bucket to be used exclusively for caching CI data, thus promoting data
isolation. The bucket should also be configured to expire older files, this way cache stored for
ephemeral branches will be discarded, hence avoiding unnecessary storage costs. Creating a
separate user that has full access only to that bucket is also a must. Easiest way to get
all of this done is with help of [terraform](https://www.terraform.io/downloads.html):

Most of the boiler plate has been taking care of by the reusable terraform module
[ci-cache-s3](https://github.com/fpco/fpco-terraform-aws/tree/master/tf-modules/ci-cache-s3). Although
not strictly required, I would recommend setting up a [keybase.io](https://keybase.io/) account,
but having a regular PGP key will do just fine. All that is necessary is creating a `main.tf` file
with this content:

```hcl
module "ci-cache" {
  source  = "github.com/fpco/fpco-terraform-aws//tf-modules/ci-cache-s3"
  prefix  = "my-cache-" # <-- make sure to set this to a custom value.
  pgp_key = "keybase:user_name" # <-- or a base64 encoded PGP public key
}

output "bucket_name" {
  value = "${module.ci-cache.bucket_name}"
}

output "access_key" {
  value = "${module.ci-cache.access_key}"
}

output "secret_key" {
  value = "${module.ci-cache.secret_key}"
}
```

Then simply running commands below will set up the S3 bucket and IAM user with permissions to access
it for you:

```
$ terraform init
$ terraform plan
$ terraform apply
```


After you apply terrafom it will deploy all of the resources and print out the bucket name,
`access_key` and an encrypted version of the `secret_key`. In order to get clear text verision of it
run:

```
terraform output secret_key | base64 --decode | keybase pgp decrypt
```

You can inspect
[ci-cache-s3/variables.tf](https://github.com/fpco/fpco-terraform-aws/tree/master/tf-modules/ci-cache-s3/variables.tf)
file for a few extra variables that can be customized in the module.


It is recommended to also setup a remote state for terraform, so it can be shared with all of your
co-workers, but that's a totally separate discussion.

_Read more on terraform if you'd like to avoid manual work in getting everything setup_:
[terraform.io](https://www.terraform.io/intro/index.html))

### CI Setup

Every invocation of `cache-s3` requires S3 bucket name and AWS credentials to be present in the
environment. Run `cache-s3 --help` to get more on that. Most common way of supplying arguments to
tools on CI is through environment variables. Here is the list of variables that are understood by
the tool:

* `S3_BUCKET` - where to store the cache (`-b`, `--bucket` cli argument)
* `AWS_ACCESS_KEY_ID` - access key
* `AWS_SECRET_ACCESS_KEY` - secret key
* `AWS_REGION` - region where the bucket is (`-r`, `--region` cli argument)
* `GIT_DIR` - used only for inferring current git branch  (`--git-dir` cli argument)
* `GIT_BRANCH` - used for namespacing the cache (`--git-branch` cli argument)

Stack specific ones:

* `STACK_ROOT` - global stack folder (`--stack-root` cli argument)
* `STACK_YAML` - path to project configuration file (`--stack-yaml` cli argument)
* `STACK_WORK` - use to rename `.stack-work` directory (`--stack-work` cli argument)

Further examples will assume all of the AWS realted variables are set.

__Important__: If the same bucket is being used for many projects, make sure to set `--prefix`
argument in order to place each of them in their own namespace and avoid cache clashes.

### Saving and restoring cache

At the end of the CI build supply all of the relative or absolute paths to directories and/or
individual files as arguments. Directories will be traversed and cached recursively:

```
$ cache-s3 save -p ~/.npm -p ~/.cabal
```

At the beginning of the build all of the files can be restored from cache simply by running:

```
$ cache-s3 restore --base-branch=master
```

Specifying base branch will let files be restored from another branch, like `master` in example
above, if current branch doesn't yet have cache of it's own.

Files and directories are restored to the exact same places on the files systems they were
before. Attributes, permissions and modification times are preserved. Symlinks are not followed, so
they are cached and restored as symlinks. _On Windows they are ignored completely!_


### Stack

For those that do not know, [stack](https://docs.haskellstack.org) is a comprehensive tool used for
developing Haskell programs. In order to avoid rebuilding any of the stack projects every time, we
need to cache these two location:

* Global stack root directory, usually located in `~/.stack`. This is used for keeping all sorts of
  files that can be reused by all projects developed by a single user.
* Folder with programs, such as GHC compiler, is usually nested inside the stack global directory,
  but can sometimes reside in a separate folder, for example on Windows.

Below is the command that can be used to cache the mentioned locations, but make sure you call it
from within your project directory, or at least supply `--stack-yaml` or `--resolver` for the
project. This way `cache-s3` will invalidate the cache if you later decide to change Stackage
resolver for your project.

```
$ cache-s3 save stack
```

Saving stack artifacts for a particular project is done in a separate step, and this is so by
design. Global stack folders rarely change, namely whenever there is a change to project
dependencies or a different resolver is being used. Local `.stack-work` folder(s) on the other hand
do change frequently with the project under active development. Here is how to cache your project:

```
$ cache-s3 save stack work
```

This will cache your `.stack-work` directory, or all of them, if your project consists of many
packages.

Restoring stack cache is just as easy as regular one:

```
$ cache-s3 restore stack
```

and

```
$ cache-s3 restore stack work
```

### Clearing

If for some reason there is a need to remove cache for particular build, all is necessary is to run
`cache-s3 clear`, `cache-s3 clear stack` or `cache-s3 clear stack work` with the same arguments that
`cache restore` would be called with. Alternatively a file can be manually removed from an S3
bucket. Despite that files on S3 have extension `.cache`, they are simple `.tar.gz` and can be
manually ispected for content, if some CI build failure debugging is necessary.

## Features

* Data will not be uploaded to S3 if it has not changed. By change here I don't mean only the
  content of files, but also attributes of files and folders, such as modification time, changes in
  permisssions or ownership. So even `touch` of one of the files being cached will trigger an upload
  to S3.
* Consistency of cache is verified when it's being restored.
* Compression algorithm is customizable. For now on Windows only gzip is available, gzip and lz4 on
  others.
* Default hashing algorithm SHA256 can also be overriden.
