# Website

This external website hosts documentation and helpful resources mainly for DOF employees in the property modeling team.

#### Overview of the website

This website is made using [quarto](https://quarto.org/), a version of markdown that is compatible with notebook formats in R (R markdown) and Python (Jupyter notebook) to display descriptions and in-line code.

First, quarto works across different languages and viewers so it needs to be downloaded separately. You can download it [here](https://quarto.org/docs/get-started/). Note that quarto cannot write to the H drive, so you must work on the C drive when using it. You can see if Quarto has been installed correctly by trying to create a new project in RStudio. If you see "Quarto Project" as an option, it has been configured correctly. If not, you may need to update your RStudio version [here](https://posit.co/download/rstudio-desktop/). 

<img src="https://github.com/user-attachments/assets/554ede74-b1fc-4991-9c87-987590d28324" style="width:600px;"/>

The site is built from the docs folder of the current repository, which contain rendered pages of the sites built by using the following files:

-   `_quarto.yml`: this source file is in the main directory and contains the configuration and settings for the whole website. Each page can have it's own settings in the yml section at the top of each page, but this file includes all settings that we want to be consistent across the entire site.

-   `*.qmd` files: each of these files represent a page of the website.

#### Updating the website

Updating or changing the website by using this tool. To make changes to the site, use the following workflow:

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

4.  Now that you are in a new branch, you can make changes to the `*.qmd` files or any other files to change the website display pages.

5.  To see how these changes would be displayed, you can click the `Render` button in RStudio or the `Preview` button in VSCode. Note: the webpage that is rendered is a static version of the `*.qmd`
files you edited and will not be changed in real time. To refresh the preview version of the webpage, close out of the preview by pressing `Stop` in the Background Jobs tab of RStudio, save your `*.qmd`
file and press render again. Iterate until you are happy with the results.

6.  Once you are happy with the changes, you need to officially render all the quarto pages to html to be displayed. Note that this must be done any time any QMD files are modified, as the Github site reads in the HTML output created by the quarto render command, and not the qmd files themselves. In the R terminal or command prompt (after navigating back to the nycdepartmentoffinance.github.io folder, if necessary), type the following:

```
quarto render
```

7. Now, both the source documents (`*.qmd`) and resulting rendered html (`docs/*.html`) should be updated. Push your changes to github as you would normally:

```
git add .
git commit -m "made updates to site"
git push
```

8. Our branch on github now contains commits past our main branch, so it's time for a pull request and merge. If you go to your repository online, you should see something like the following. Click on **Compare & pull request**

<img src="https://github.com/user-attachments/assets/d7870df4-5145-40bf-b3a2-e48a657ec2e1" style="width:600px;"/>

9. At the next screen, click on **Create pull request**

<img src="https://github.com/user-attachments/assets/b10add6e-6b02-4e7a-add9-63353bafca73" style="width:600px;"/>

10. Next, click on **Merge pull request**

<img src="https://github.com/user-attachments/assets/c04b89b6-1dd2-4c8c-83a0-b3e297dcbbe5" style="width:600px;"/>

Note: You may encounter an error indicating that there are conflicts that need to be resolved via the command line. If this happens, delete your "docs" folder and re-run quarto render. Then add, commit, and push again. This should resolve the issue. 

Now, github will automatically start an action (on any push to the `main` branch) that will re-render our live website. 

Our changes are complete!
