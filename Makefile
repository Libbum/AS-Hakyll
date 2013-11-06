clean: site
	./site clean

deploy: clean rebuild
	./site deploy

post:
	touch posts/`date +%Y-%m-%d`-${TITLE}.markdown

preview: rebuild
	./site watch

rebuild: site
	./site rebuild

site: site.hs
	ghc --make site.hs
