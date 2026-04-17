---
name: mgunit release checklist
about: Checklist for an mgunit release
title: 'Release mgunit X.Y.Z'
labels: release
assignees: mgalloy

---

### Pre-release check

- [ ] add date to version line in `RELEASES.md`
- [ ] check that version to release in `RELEASES.md` matches version in `CMakeLists.txt`

### Tag release

- [ ] tag with `git tag -a vX.Y.Z` and comment "Release version X.Y.Z"
- [ ] push tags with `git push --tags`

### Update wiki

- [ ] configure with `example_configure`
- [ ] cd into `build`
- [ ] build with `make`
- [ ] build package with `make package`
- [ ] copy `mgunit-X.Y.Z.tar.gz` to `releases` subdirectory of wiki repo
- [ ] update `Released.md` in the wiki repo

### Install at `updates.idldev.com`

- [ ] untar `mgunit-X.Y.Z.tar.gz`, change name of directory to `mgunit`, and zip
- [ ] update the package with `scp mgunit.zip idldev.com:/home/mgalloy/packages.idldev.com/mgunit.zip`
