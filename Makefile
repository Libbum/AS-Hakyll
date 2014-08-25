# If cygwin gives you issues with UTF-8, run .scripts/codePage
ifeq ($(OS), Windows_NT)
        exe := ./site.exe
else
        exe := ./site
endif

clean: site
	$(exe) clean

deploy: clean rebuild
	$(exe) deploy

post:
	echo -e '---\ntitle: '${TITLE}'\n---\n\n' >  posts/`date +%Y-%m-%d`-${TITLE}.markdown
	vim posts/`date +%Y-%m-%d`-${TITLE}.markdown

preview: rebuild
	$(exe) watch

rebuild: site
	$(exe) rebuild

site: site.hs
	ghc --make -threaded site.hs
