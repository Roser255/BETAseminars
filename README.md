# BETAseminars

This is the GitHub repository for testing RStudio trainings at BETA TC. 

We will start Monday 29th of July 2024 with a seminar session entitled:

### Statistical and visual analyses of your experimental data: working with RStudio connected to GitHub
With Miquel Bistué MBR data

*Last updated: July 2024*

-------------------------------------------------------

Before you contribute, read the information below:

# SET-UP STEPS  
## 1 Link your local Rstudio to the online repository  
Having an online GitHub **repository** is useful to share scripts and data and avoid versioning issues. We won't have to remember who has the latest version of the script and save previous bits of codes, as GitHub keeps in memory the different edits (similar to what GoogleDocs does).

Currently, we all have a local Rstudio or R folder where we work on the corresponding data - the idea is that we will connect this local folder to the online **repository**.

Unless you have another routine already, you can download [GitHub Desktop](https://desktop.github.com/), and use it to commit, push and pull from the online repository. GitHub Desktop is to Git what Rstudio is to R: an user-friendly interface. (note that you can also use the built-in commit tool of Rstudio)

The first time, you will have to **clone** the online repository. This step has to be done only once. Cloning creates a local image of this online repository `https://github.com/calderom/BETAseminars`. Note that this repository is public, so everyone can see what is in it. For more sensitive data we could have created a private repository and "invite" collaborators. 

You will then edit your scripts as you would usually do (in your R project), but once you're done, you can **commit** your changes, and **push** them to the online repository. Other people in the repository can **pull** from the online repository and get the latest version of what you've been working on.

<u>Vocabulary:</u>  repository, clone, commit, push, pull  

<u>Ressources:</u>  

* [Here](https://docs.github.com/en/desktop/contributing-and-collaborating-using-github-desktop/adding-and-cloning-repositories/cloning-and-forking-repositories-from-github-desktop) are the steps to clone a repository.  
* [Here](https://docs.github.com/en/desktop/contributing-and-collaborating-using-github-desktop/making-changes-in-a-branch/committing-and-reviewing-changes-to-your-project) are the steps to commit local changes.  
* [Here](https://docs.github.com/en/desktop/contributing-and-collaborating-using-github-desktop/making-changes-in-a-branch/pushing-changes-to-github) are the steps to push local changes.  
* You can pull remote changes from the online repository by pressing the same button than when you push.

## 2 Structure of the folder  
### 2.1 Input
Input has data needed in the R scripts. 

### 2.2 Output
**Output > Data**: where you can save datasets you created. 
**Output > Figures**: where we'll have our figures output. 

### 2.3 R
Where the scripts are. Each collaborator can create a sub-folder with their name to keep their scripts organized. See the next point: "stay in your lane".

## 3 "Stay in your lane" 
The "stay in your lane" approach is used in big Data Harmonization groups, and it's been working great so far. Basically, the idea is that we have a common repository, making it easy to share bits of code, but we edit only our scripts. We can avoid merging "conflicts", which is a nice feature of GitHub but can get messy, specially if not all the collaborators have the same expertise level. In the script folder (`R`), create a subfolder with your lastname_firstname (e.g., caldero_maria). If you can, try also to name your scripts within that folder consistently so we can easily trace back what the script does.

## 4 .gitignore
The file .gitignore lists all the extension you want Git to ignore in its commit. 
It is useful in at least two cases: (1) Avoid having very large commits after each working session and (2) have working scripts/figures/datasets, and have scripts/figures/datasets that we want to share with the group.  

### 4.1 Scripts
Most of the scripts can be shared, because they are not large objects. However, if for some reasons, you do not want to accidentally share a specific script, just change the extension from `.R` to `donotshare.R` (e.g., `script_zoop.R` to `script_zoop_donotshare.R`).

### 4.2 Datasets and figures
I set up the .gitignore so that if you create a folder `donotshare` in `Output/Data` or `Output/Figures`, the files within the folder will not be listed in the diff. Place in there any dataset or figure that you do not want to share with the group.

*Note: these good practices have been taken from <a href="https://rosalieb.github.io/rosaliebruelweb/index.html" target="_blank">Rosalie Bruel</a> (OFB, France). Follow her work to learn more!*

# OTHER GitHub RESOURCES
These GitHub resources were listed by the [GLEON-ZIG](https://gleon.org/research/projects/zooplankton-indicators-group-zig) project's champions:   

* [GLEON's GitHub workshop](https://www.youtube.com/watch?v=B-FHx4l1BNU) by Jem Stachelek
* [Jenny Bryan’s Happy Git with R:](https://happygitwithr.com/big-picture.html) Chapter 1 introduces Git and its benefits.
* [GitHub for Project Management by Openscapes:](https://openscapes.github.io/series/github-issues.html) Chapter 10 includes an overview of Git Issues.
