import argparse
import os

import git
import github
import semver

whoami = "release"
def env(var, default=None):
    return os.environ.get("RELEASE_" + var, default)

import logging
logger = logging.getLogger(whoami)

def parse_args():
    parser = argparse.ArgumentParser(
            description = "release tool",
            formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument("--log", default=env("LOG_LEVEL", "WARN"), help="set log level")

    parser.add_argument("--github-repo", default=env("GITHUB_REPOSITORY", os.environ.get("GITHUB_REPOSITORY")))
    parser.add_argument("--local-repo", default=env("LOCAL_REPOSITORY", os.environ.get("GITHUB_WORKSPACE")))
    # ought to be faster to make a local clone:
    # * want to check the .version files
    # * my repositories are quite small
    # * need to traverse down parent, and log and ...

    return parser.parse_args()

def setup_logger(level):
    logger.setLevel(level)

    ch = logging.StreamHandler()
    ch.setLevel(level)

    f = logging.Formatter(fmt="%(asctime)s:%(name)s:%(levelname)s %(message)s", datefmt="%Y-%m-%dT%H:%M:%S%z")
    ch.setFormatter(f)

    logger.addHandler(ch)

def mk_github(args):
    token = os.environ.get("GITHUB_TOKEN")
    if token is not None:
        auth = github.Auth.Token(token)
    else:
        auth = None
    return github.Github(auth=auth)

def get_release_to_tag(args):
    repo = mk_github(args).get_repo(args.github_repo)

    releases = {}
    for r in repo.get_releases():
        if r.draft:
            continue

        releases[r.tag_name] = {
            "name": r.name,
            "prerelease": r.prerelease,
        }
        break # HACK
    return releases

def sandbox(args):
    if args.local_repo is None:
        raise NotImplementedError("clone repo")
    repo = git.Repo(args.local_repo)

    releases = get_release_to_tag(args)

    for tag_name, r in releases.items():
        r["commit"] = repo.tag(tag_name).commit

    print(releases)

def main():
    args = parse_args()
    setup_logger(args.log.upper())
    logger.debug(f"args: {args}")

    sandbox(args)

if __name__ == "__main__":
    main()
