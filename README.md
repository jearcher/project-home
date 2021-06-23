## Modeling Eviction Risk in CA

### Data

Some data is pulled from the Urban Displacement Project's HPRM private [repo](`https://github.com/urban-displacement/hprm.git`).

Census data is pulled from `tidycensus()` API in `R`.

That data is stored into a folder called `data` which is not stored on github.

### Website

1. From the root directory of this repo, run `cd website/server/HTML`.

2. Ensure either node or npm is installed by running `node -v` or `npm -v`. (If neither has been installed yet, go [here](https://www.npmjs.com/get-npm).)

3. Run `npm install --legacy-peer-deps` to install node modules.

4. Finally, **to launch the server** via localhost, run `npm run serve`.
