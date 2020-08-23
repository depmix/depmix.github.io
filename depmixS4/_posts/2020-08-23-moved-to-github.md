---
layout: post
title: Moving depmixS4 development to GitHub
category: depmixS4
author: maarten
---

We have migrated development of depmixS4 from
[RForge](https://r-forge.r-project.org/projects/depmix/) to
[GitHUb](https://github.com/depmix/depmixS4)!
While in the end this turned out
to be relatively straightforward, it took a little
puzzling and a lot of internet searching. We ideally wanted to keep the commit
history and releases/tags to remain intact on GitHub. Moreover, we wanted to
migrate parts of the svn repository to separate git repositories. For instance,
the RForge svn repository contained subfolders for `depmixS4` as well as the
legacy R package `depmix`, and these need separate repositories on GitHub. For
example, relevant subfolders in the RForge repository were:

```
papers/
    jss/
pkg/
    depmix/
    depmixS4/
tags/
    depmix-0.9.8/
    ...
    release-1.3.2/
```

I first tried to use `svn2git`, but didn't succeed in connecting to the RForge repository
via ssh. So I went with `git-svn`. I looked at various websites, but in the end
I think I mainly followed advice from [here](http://www.sailmaker.co.uk/blog/2013/05/05/migrating-from-svn-to-git-preserving-branches-and-tags-3/)
and [here](https://stackoverflow.com/questions/79165/how-do-i-migrate-an-svn-repository-with-history-to-a-new-git-repository).
I created an `svnauthors.txt` file to transfer commits from the svn over to GitHub. The contents of this file were roughly

```
usename1 = FirstName1 LastName1 <email@users.noreply.github.com>
username2 = FirstName2 LastName2 <email@users.noreply.github.com>
```

where of course you would need to replace these with real names and email
addresses. Then, to migrate the `depmixS4` part to GitHub, in the terminal, I
used

```
mkdir svn-to-github
cd svn-to-github
git svn clone svn+ssh://username@svn.r-forge.r-project.org/svnroot/depmix/ --trunk pkg/depmixS4 --tags tags/release*  --authors-file svnauthors.txt
```
Note that I put the required subfolder as the `trunk` argument, and used the
`tags` argument to include only tags starting with `release` (in order to filter
out the tags for `depmix`). This took some time and multiple password entries,
but it seemingly worked! In the `svn-to-github` folder, there was now a folder
called `depmix` folder (note that the name is taken from the svn address, not the trunk argument)
with an initialized git repository. Some further work is needed to transfer
the svn tags to git tags, so I did the following:

```
mv depmix depmixS4 // rename folder
cd depmixS4
git branch -a // check for branches
// turn branches into tags
for tag in `git branch -r | grep "tags/" | sed 's/ tags\///'`; do
  git tag -a -m"Converting SVN tags" $tag refs/remotes/$tag
done
git tag // show the tags
```

I did some further tests to see whether the repository was working as intended
(again following [this](http://www.sailmaker.co.uk/blog/2013/05/05/migrating-from-svn-to-git-preserving-branches-and-tags-3/) advice), which seemed to be the case.
Then I created an empty repository on GitHub called `depmixS4`, ready to receive
the local git repository:

```
git remote add origin https://github.com/depmix/depmixS4.git
git push --all
git push --tags // tags are not included in all
```

And voila, a working git repository or GitHub with the commit history and tags
from RForge. Fantastic!
