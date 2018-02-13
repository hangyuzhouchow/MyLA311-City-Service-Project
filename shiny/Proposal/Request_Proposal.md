# 311 Service Request Proposal
Ying Chen

## What

Other than learning about the distribution/frequency of different service, I'm thinking of helping 311 to better serve people, in other words,  **better solve requests**.


## Why

At the very begining, we may first figure out the function of 311 call:
> MyLA311 links Angelenos with the services and information they need to enjoy their city, beautify their community and stay connected with their local government.

> The ‘Submit Service Request’ feature allows you to quickly and easily request the City’s most popular services, including graffiti removal, pothole repair, and bulky-item pickup.

Besides providing infomation for citizens, 311 acts as the bridge between people and different departments to better serve and solve issues within the city.

Thus, the main task for 311 on service requests part is to make sure all the requests solved well.

## How
We need to break down our goal: solve requests well into **2 parts**:

### 1. Get Things Done !

This may sound silly, but there must be requests that reported as "closed/solved" that are requested again within a short period.

Those requests are really important because issues happening again and again or problem reported but never getting done would make people feel frustrated.

To look deep into it, we need to define those cases that are:

- the same service type
- requested during small time interval
- at the close/same address

#### Goal

In order to decrease the "re-Request Rate", we should figure out: 

- what kind of requests are re-requested

- which area get a lot of requests


### 2. Solve Fast ! 

Not only do we want things getting done, but also want them to be done quickly.

When we talk about *fast*, we are refering to the services that has been closed, and the time used could be mutated by substracting the updated time from the created time.


#### What service is slow ?

#### Resources Enough ?

There may be lots of reason behind a slow solution, but we'll just dig into the resources.

By connecting/merging data from [CensusReport](http://censusreporter.org/), we take into account both the info of a area(difined by the zipcode or others) such as population, square miles, housing, etc. and the sloving rate to conjecture whether there are enough sources to satisfying the need in that area.