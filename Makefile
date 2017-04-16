# If cygwin gives you issues with UTF-8, run .scripts/codePage
# If using Arch-Haskell, it isn't built with a previewer.
ifeq ($(OS), Windows_NT)
        exe := ./site.exe
        watch := ./site.exe watch
else
        exe := ./site
        watch := cd _site; python -m http.server 8000
endif

clean: site
	$(exe) clean

deploy: clean rebuild
	$(exe) deploy

post:
	echo -e '---\ntitle: '${TITLE}'\n---\n\n' >  posts/`date +%Y-%m-%d`-${TITLE}.markdown
	nvim posts/`date +%Y-%m-%d`-${TITLE}.markdown

preview: rebuild
	$(watch)

rebuild: site
	$(exe) rebuild

site: site.hs
	ghc --make -threaded site.hs

crypto:
	sudo ghc-pkg hide cryptonite-0.22
