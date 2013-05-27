all: build

build: hakyll
	./site build

hakyll: site.hs
	export GHC_PACKAGE_PATH=/usr/local/Cellar/ghc/7.4.2/lib/ghc-7.4.2/package.conf.d/:/Users/geoffroycouprie/dev/ios/pilotssh.com/cabal-dev/packages-7.4.2.conf/ && ghc --make site.hs
	./site clean

new:
	@./new_post.sh

publish: build
	git add .
	git stash save
	git checkout publish
	find . -maxdepth 1 ! -name '.' ! -name '.git*' ! -name '_site' -exec rm -rf {} +
	find _site -maxdepth 1 -exec mv {} . \;
	rmdir _site
	git add -A && git co "Publish" || true
	git push
	git push clever publish:master
	git checkout master
	git clean -fdx
	git stash pop || true

preview: hakyll
	./site clean
	./site preview -p 9000

clean: hakyll
	./site clean
	rm -f site

check: hakyll
	./site check
