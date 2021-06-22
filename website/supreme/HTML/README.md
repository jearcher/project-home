# Supreme

HTML landing page template.

* [Getting started](#getting-started)
* [Deploy with Netlify](#deploy-with-netlify)

## Getting started
* First, ensure that node.js & npm are both installed. If not, choose your OS and installation method from [this page](https://nodejs.org/en/download/package-manager/) and follow the instructions.
* Next, use your command line to enter your project directory.
* This template comes with a ready-to-use package file called `package.json`. You just need to run `npm install` to install all of the dependencies into your project.
* When `npm` has finished with the install, run `npm run build` to generate the output files into the `public` folder.

You're ready to go! Run any task by typing `npm run task` (where "task" is the name of the task in the `"scripts"` object). The most useful task for rapid development is `npm run serve`. It will start a new server, open up a browser and watch for any SCSS or JS changes; once it compiles those changes, the browser will automatically inject the changed file(s)!

### Compiles and hot-reloads for development
```
npm run serve
```

### Compiles and minifies for production
```
npm run build
```

## Deploy with Netlify
Premium HTML templates come with a ready-made `netlify.toml` file to allow you deploying with Netlify and go live in a few clicks. You just need to create a repository and copy the whole content of the `HTML`folder. Then, create a new site from Git in Netlify and deploy the app.