# NYC Department of Finance Github Website

This repository is the source for all content hosted on the [NYC Department of Finance Github Website](https://nycdepartmentoffinance.github.io/), which hosts documentation and helpful resources for DOF employees in the property modeling team. In terms of structure, this project is built using **Quarto**  and is automatically published using **GitHub Pages**. 

## How does it work?

[GitHub Pages](https://pages.github.com/) is a free and convenient hosting solution for static websites. GitHub Pages works by serving HTML, CSS, and JavaScript files directly from a branch of a GitHub repository (in this case: main). When we push changes to the main branch of this repository, GitHub automatically rebuilds and publishes the site, using the contents of that branch as the website source.

But how do we build those HTML pages? That's where Quarto comes in. [Quarto](https://quarto.org/) is a modern, open-source publishing system that turns source files (like `.qmd`, `.md`, or notebooks) into static output formats such as HTML, PDF, or Word. It renders content using a combination of markdown, embedded code (R, Python, Julia, etc.), and templates.

So how do these pieces work together? In essence, we can follow the steps to make any new content on our website (reproducibly, and for free):

**Workflow:**
1. Git clone this project to your local machine.
2. Add or modify content content in `.qmd`, `.md`, or `.ipynb` files, locally.
3. Use Quarto to render the site locally, which generates the html files from the notebooks.
4. Preview the website locally to confirm the changes you made.
5. Push changes of your branch of the repository to GitHub (`git add .`, `git commit -m "message"`, `git push`).
6. Create pull request (merge) to the main branch (approving when reviewed and does not have any breaking changes).
7. Push changes to main branch.
8. GitHub Pages automatically re-publishes the newly rendered `docs/` directory as the new version of the public website.

#### Getting set up

First, quarto works across different languages and viewers so it needs to be downloaded separately. You can download it [here](https://quarto.org/docs/get-started/). Note that quarto cannot write to the H drive, so you must work on the C drive when using it. You can see if Quarto has been installed correctly by trying to create a new project in RStudio. If you see "Quarto Project" as an option, it has been configured correctly. If not, you may need to update your RStudio version [here](https://posit.co/download/rstudio-desktop/). 

<img src="https://github.com/user-attachments/assets/554ede74-b1fc-4991-9c87-987590d28324" style="width:600px;"/>

#### Clone a local version of this repository and make a new branch for changes

1. After setting up Git using the guide found [here](https://github.com/nycdepartmentoffinance/onboarding/blob/main/github.md), clone this repository using the following code in the Command Tool:
   
```         
git clone https://github.com/nycdepartmentoffinance/nycdepartmentoffinance.github.io
```

2. Navigate into the repo folder you just cloned using the following code. If you navigate out of this folder at any point, either by using the change directory command or by setting a different working directory in your R project, be sure to navigate back to this new nycdepartmentoffinance.github.io parent folder before running the "quarto render" command.
 
```         
cd nycdepartmentoffinance.github.io
```
  
3. Check out a new branch of the repository, do not make edits directly in `main`. For more on git branching, read [here](https://git-scm.com/book/en/v2/Git-Branching-Branches-in-a-Nutshell). You can do this by either making a new issue describing what you want to change and creating a branch from the new issue, or simply by typing the following in your console in the home directory of your repository (folder):

```         
git checkout -b [NEW_BRANCH_NAME]
```

NOTE: If you have already created a branch and wish to resume working on it, you can type this code instead to fetch the most recent version of the repo and then resume your work:

```
git pull main        
git checkout [YOUR_EXISTING_BRANCH_NAME]
```

4. Use renv to pull in all the packages needed in order to work with the site. First, download renv as an R package in your RStudio console:
```
install.packages("renv")
```

5. Once renv has successfully downloaded, use `renv::restore()` to download all the packages needed for the project using the existing renv.lock file in the repository:
```
renv::restore()
```
The renv.lock file is basically a list of packages that need to be downloaded. Calling `renv::restore()` tells R to build an environment identical to the one we used to build the site to make sure everything is consistent. 

At the end of this process, the packages from the lock file should all be stored in a folder similar to this `.../nycdepartmentoffinance.github.io/renv/library/windows/R-4.4/x86_64-w64-mingw32`. You should have a list of folders that looks like this:

![image](https://github.com/user-attachments/assets/65e1ef4b-db8b-490d-89ed-72e917ddb01c)

You can double check to make sure that it worked by calling the following in your Console:
```
renv::status()
```

Great! Now you have a local version of the website and a replicated environment with all of the packages you need.

#### A note on the site structure

Now that you have a local version of the repository of the site, you can examine the site structure a bit more. The site is built from the docs folder of the current repository, which contain rendered pages of the sites built by using the following files:

-   `_quarto.yml`: this source file is in the main directory and contains the configuration and settings for the whole website. Each page can have it's own settings in the yml section at the top of each page, but this file includes all settings that we want to be consistent across the entire site.

-   `*.qmd` files: each of these files represent a page of the website. Note: there are *.qmd pages nested within the `models/` folder and the `data_dictionary/` folder as these are groups of pages that can have shared settings.

#### Updating the website

To make changes to the site, use the following workflow:

1. Confirm you are in a new branch of the repository. 

```
git status
```

The first line of the git status response should be `On branch YOUR_BRANCH_NAME`

2.  Now that you are in a new branch, you can make changes to the `*.qmd` files or any other files to change the website display pages. To add interactive maps in R, explore what [leaflet](https://rstudio.github.io/leaflet/) has to offer. But explore any other packages you want! Anything that can be rendered in a markdown file can be displayed on the website.

3.  To see how these changes would be displayed on the website, you can click the `Render` button in RStudio or the `Preview` button in VSCode. Note: the webpage that is rendered is a static version of the `*.qmd`. The files you edited and will not be changed in real time. To refresh the preview version of the webpage, close out of the preview by pressing `Stop` in the Background Jobs tab of RStudio, save your `*.qmd` file and press render again. Iterate until you are happy with the results.

4.  Once you are happy with the changes, you need to officially render all the quarto pages to html to be displayed. Note that this must be done any time any QMD files are modified, as the Github site reads in the HTML output created by the quarto render command, and not the qmd files themselves. In the R terminal or command prompt (after navigating back to the nycdepartmentoffinance.github.io folder, if necessary), type the following:

```
quarto render
```
5.  After rendering, you can look at the complete version of the updated site by typing the following in the R terminal:
```
quarto preview
```

5. Now that you have rendered your work and checked in the preview version that it is displaying correctly, both the source documents (`*.qmd`) and resulting rendered html (`docs/*.html`) should be updated. Push your changes to github as you would normally:

```
git add .
git commit -m "made updates to site"
git push
```

6. Our branch on github now contains commits past our main branch, so it's time for a pull request and merge. If you go to your repository online, you should see something like the following. Click on **Compare & pull request**

<img src="https://github.com/user-attachments/assets/d7870df4-5145-40bf-b3a2-e48a657ec2e1" style="width:600px;"/>

7. At the next screen, click on **Create pull request**

<img src="https://github.com/user-attachments/assets/b10add6e-6b02-4e7a-add9-63353bafca73" style="width:600px;"/>

**Note:** you might get a [merge conflict](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/addressing-merge-conflicts/resolving-a-merge-conflict-using-the-command-line) at this step. If all of the conflicts are in the `docs/` folder of the repository, the easiest thing to do is to delete the entire docs folder, re-render your version of the website (`quarto render`) and then commit the changes. Typically we wouldn't want to delete full folders (it may seem counter-intuituve), but it's fine in this case because quarto re-builds this folder everytime the site is rendered. This is the folder that is displayed with github pages, but all the source material used by quarto to generate the site is in other folders so this step is perfectly fine as a reset. If you have any other questions about merge conflicts, don't hesitate to ask Claire.

8. Now that the pull request is completed, message Claire on Teams or tag her in the pull request to get a quick review before pushing the changes to the live website. This check is mainly to ensure that everything is rendered correctly and that pushing the changes to the live site will not change anything else unexpectedly. Once she approves, then you can proceed with the merge.

9. Next, click on **Merge pull request**

<img src="https://github.com/user-attachments/assets/c04b89b6-1dd2-4c8c-83a0-b3e297dcbbe5" style="width:600px;"/>

Now, github will automatically start an action (on any push to the `main` branch) that will re-render our live website. 

Our changes are complete!
