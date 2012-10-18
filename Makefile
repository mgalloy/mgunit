VERSION=1.3dev
REVISION=r`svn info | sed -n 's/Revision: \(.*\)/\1/p'`
RELEASE="$(VERSION)-$(REVISION)"
IDL_DOCGEN=idl
IDL_SAVGEN=idl64
TAG=MGUNIT_`echo $(VERSION) | sed -e"s/\./_/g"`

.PHONY: all doc userdoc webdoc clean dist srcdist version tag branch
 
all:
	cd src; make

doc:
	$(IDL_DOCGEN) -e mgunit_build_docs

userdoc:
	$(IDL_DOCGEN) -e mgunit_build_userdocs

webdoc:
	$(IDL_DOCGEN) -e mgunit_build_userdocs
	scp -r api-userdocs/* idldev.com:~/docs.idldev.com/mgunit

clean:
	rm -rf api-docs
	cd src; make clean

version:
	sed "s/version = '.*'/version = '$(VERSION)'/" < src/mgunit_version.pro | sed "s/revision = '.*'/revision = '$(REVISION)'/" > mgunit_version.pro
	mv mgunit_version.pro src/

# need to: blow away old dist, blow away api-docs, generate api-docs
dist:
	rm -rf mgunit-$(RELEASE)
	rm -rf api-userdocs/

	make version

	make userdoc

	$(IDL_SAVGEN) -IDL_STARTUP "" mgunit_build

	mkdir mgunit-$(RELEASE)/

	mv mgunit.sav mgunit-$(RELEASE)/

	cd docs; make
	svn export docs mgunit-$(RELEASE)/docs/
	cp docs/using-mgunit.pdf mgunit-$(RELEASE)/docs/
	cp docs/using-mgunit.html mgunit-$(RELEASE)/docs/
	rm mgunit-$(RELEASE)/docs/Makefile

	cp -r api-userdocs mgunit-$(RELEASE)/
	cp src/error_is_fail.pro mgunit-$(RELEASE)/
	cp src/error_is_pass.pro mgunit-$(RELEASE)/
	cp src/style.css mgunit-$(RELEASE)/
	cp src/mgunit-templates.xml mgunit-$(RELEASE)/
	cp COPYING mgunit-$(RELEASE)/
	cp RELEASE mgunit-$(RELEASE)/	

	zip -r mgunit-$(RELEASE).zip mgunit-$(RELEASE)/*
	rm -rf mgunit-$(RELEASE)

srcdist:
	rm -rf mgunit-src-$(RELEASE)
	rm -rf api-docs/

	make version
	make doc

	mkdir mgunit-src-$(RELEASE)/

	cd docs; make
	svn export docs mgunit-src-$(RELEASE)/docs/
	cp docs/using-mgunit.pdf mgunit-src-$(RELEASE)/docs/
	cp docs/using-mgunit.html mgunit-src-$(RELEASE)/docs/
	
	cp -r api-docs mgunit-src-$(RELEASE)/
	cp src/*.pro mgunit-src-$(RELEASE)/
	cp src/style.css mgunit-src-$(RELEASE)/
	cp src/dist_tools/mg_src_root.pro mgunit-src-$(RELEASE)/
	cp src/dist_tools/mg_options__define.pro mgunit-src-$(RELEASE)/
	cp src/dist_tools/collection/mgcohashtable__define.pro mgunit-src-$(RELEASE)/
	cp src/dist_tools/collection/mgcoarraylist__define.pro mgunit-src-$(RELEASE)/
	cp src/dist_tools/collection/mgcoabstractlist__define.pro mgunit-src-$(RELEASE)/
	cp src/cmdline_tools/mg_ansicode.pro mgunit-src-$(RELEASE)/
	cp src/mgunit-templates.xml mgunit-src-$(RELEASE)/
	cp COPYING mgunit-src-$(RELEASE)/
	cp RELEASE mgunit-src-$(RELEASE)/	

	zip -r mgunit-src-$(RELEASE).zip mgunit-src-$(RELEASE)/*
	rm -rf mgunit-src-$(RELEASE)

tag:
	make_tag.sh $(TAG) tags

branch:
	make_tag.sh $(TAG) branches
