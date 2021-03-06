## COMP6235: Foundation of Data Science (2017-2018) Course Work 3
###### This is the project of from the kaggle competition
###### Group name : H 
###### Group member    : <br>

|Members name | Members email|
|:------------: | :-------------:|
|Bin Qian     | bq1y17@soton.ac.uk|
|Jie Su       | js2m17@soton.ac.uk|
|**Xinyu Hu**     | xh2n17@soton.ac.uk|
|Junjie Lu    | jl9n17@soton.ac.uk|
|Shuo Wang    | sw4m17@soton.ac.uk|


## Before running!!!
## Download Data
* drop the **get_dataset.sh** file into the command line or Terminal to auto download the data file([Homebrew](https://brew.sh) should be set before)
* If there are something wrong with above, you can try to download the **[data](https://www.dropbox.com/s/aecihu4d566su4q/data.zip?dl=0)** file from the drop box (Github limits the upload file size).<br>
After download the data, please decompress the data file and put it under the pythonKaggle directory.(e.g. pythonKaggle/data)

## Requirements
To replicate the findings and execute the code in this repository you will need basically the next Python packages:<br>
You can use "[condo](https://conda.io/docs/user-guide/install/index.html)" or "[pip](https://pip.pypa.io/en/stable/installing/)" to install those python package<br>
* [NumPy](http://www.numpy.org)
* [Pandas](http://pandas.pydata.org)
* [Jupyter Notbook](http://jupyter.org) **("The Notebook environment should set up as Python 3.6")**
* [SciKit-Learn](http://scikit-learn.org/stable/)
* [Matplotlib](http://matplotlib.org)
* [XGBoost Installation](https://xgboost.readthedocs.io/en/latest/build.html)**(take about 15mins, little bit complex in windows system)**




## Description
New users on Airbnb can book a place to stay in 34,000+ cities across 190+ countries. By accurately predicting where a new user will book their first travel experience, Airbnb can share more personalized content with their community, decrease the average time to first booking, and better forecast demand.<br>


In this competition, the goal is to predict in which country a new user will make his or her first booking. There are 12 possible outcomes of the destination country and the datasets consist of a list of users with their demographics, web session records, and some summary statistics.

## Running Environment
* MacBook Pro (Retina, 13-inch, Late 2013)
* Processor 2.4 GHz Intel Core i5
* Memory 8 GB 1600 MHz DDR3
* Graphics Intel Iris 1536 MB


## Check the accuracy
* Please check the accuarcy in the **[submission](https://www.kaggle.com/c/airbnb-recruiting-new-user-bookings)** site of Kaggle competition
* If you haven't registered, then try the login account below:
* Account ID: **602670464@qq.com**
* Password: **antelope0718**
* The image shows below is our submit detail:<br>
![alt text][submit]

[submit]: https://github.com/PBDexter17/DS_Group_Coursework/blob/master/pythonKaggle/Markdown_img/submit_detail.png "Logo Title Text 1"


## Connect MongoDB to Visualisation tool (Tableau)
* Downloading the **[BI tool](https://www.mongodb.com/download-center#bi-connector)** and **[MongoDB](https://www.mongodb.com)**
* Open MongoDB and use "mongodrdl -d mydb -c airbnb schema.drdl" to create schema 
* Open Server"--host 127.0.0.1:27017 --username Mymongo --password 12345 -db mydb --colloction airbnb --authenticationDatabase admin --out schema.drdl"
* Cnonnecting: "mongosqld.exe --schema schema.drdl" 
* Connect to Tableau:<br> 
![alt text][logo]

[logo]: https://github.com/PBDexter17/DS_Group_Coursework/blob/master/pythonKaggle/Markdown_img/Picture1.png "Logo Title Text 2"

## Tableau Visualisation Link
This is the visualisation **[link](https://public.tableau.com/views/map3_10/Sheet1?:embed=y&:display_count=yes&publish=yes)** in the Tableau of our project




## Resources
* [XGBoost Documentation](https://xgboost.readthedocs.io/en/latest/) - A library designed and optimized for boosted (tree) algorithms.

