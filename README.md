# Project Title

Real Estate Web Scraper in Haskell

## Description

A simple concurrent real estate web scraper. This scraper is written in [Haskell](https://www.haskell.org/). It is a simple web scraper that uses the [Haskell Conduit Downloader](https://hackage.haskell.org/package/http-conduit-downloader) library to make HTTP requests and the [Scalpel](https://hackage.haskell.org/package/scalpel) library to extract information from HTML pages. ATwo implementations of a cache and datalake, using [STM](https://hackage.haskell.org/package/stm) and postgresql using [Postgres Simple](https://hackage.haskell.org/package/postgres-simple) exist. The scraper is concurrent with each work started in a seperate thread.

Currently the scraper can only scrape the [PropertyBook ZW](https://www.propertybook.co.zw/) website. The scraper can easily be extended to scrape other websites.

## Getting Started

### Dependencies

- Should work anywhere you can use cabal
- [Haskell](https://www.haskell.org/)
- [Cabal](https://www.haskell.org/cabal/)
- [Postgres](https://www.postgresql.org/)
- [Postgres Simple](https://hackage.haskell.org/package/postgres-simple)
- [Haskell Conduit Downloader](https://hackage.haskell.org/package/http-conduit-downloader)
- [Scalpel](https://hackage.haskell.org/package/scalpel)
- [STM](https://hackage.haskell.org/package/stm)
- [UnliftIO](https://hackage.haskell.org/package/unliftio)

### Executing program

- Still a work in progress
- You might need to take a look at the code to see how to use it in case you're early here.

```
$ cabal repl
λ> pbc <- newPropertyBookInMemoryCrawler
λ> import Crawler.Simple
λ>
λ> runCrawler pbc "https://www.propertybook.co.zw/"
λ>
λ> stopCrawler pbc
```

## Help

All help is welcome. If you have any suggestions, please open an issue or a pull request.

## Authors

Contributors names and contact info

ex. Trevor Sibanda
ex. [@trevorsibanda](https://github.com/trevorsibanda)

## Version History

- 0.0.1
  - Initial Release

## License

This project is licensed under the GNU GPLv3 License
