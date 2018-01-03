# Chicago-dataset-Triptime
This project was completed as a part of IDS 575 coursework.

Aim:To improve the efficiency of taxi electronic dispatch system by predicting the trip time. If the dispatcher knew the approximate end time and drop off location of the current ride, the driver can be assigned to the next trip appropriately. This way we can eliminate any trip lag time between rides. This increases the profitability for the service provider and reduces the booking time for the customer by providing driver details instantaneously. Building a predictive framework that can infer the trip time of taxi rides in Chicago is the main aim of the project.

The data is picked from https://data.cityofchicago.org/Transportation/Taxi-Trips/wrvz-psew

The following procedure will be followed for the analysis:
•	Training and Test Data: Split the available data into training and test data sets.

•	Data cleaning: Cleaning the Training data will include removing missing values.

•	Data preprocessing: Few columns in the data will have to be changed from the existing data type to a different data type depending on the model requirements. A few extra columns might be required to analyze data based on the day of the week (Weekday, Weekend), time of the day(morning, afternoon, evening, night) etc.

•	Relationship between dependent variables and independent variable: My target variable is trip time. I need to study the dependency between the variables and determine which analysis method is suitable.

•	Model building: The idea is to explore which method fits the best. Models such as Linear, Ridge, LASSO, Random Forest, Boosting are implemented and their accuracy compared.

•	Prediction: I will use RMSLE (Root mean squared Log error) to evaluate the different models.
