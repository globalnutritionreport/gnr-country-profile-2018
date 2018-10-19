#!/usr/bin/python
# -*- coding: utf-8 -*-

from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.platypus import Paragraph
from reportlab.lib.enums import TA_CENTER
import copy
import pandas as pd
import pdb
import numbers
import re

style = getSampleStyleSheet()
whiteParaStyle = ParagraphStyle('whiteParaStyle',parent=style['BodyText'],textColor="white",alignment=TA_CENTER)
greyParaStyle = ParagraphStyle('greyParaStyle',parent=style['BodyText'],textColor="#443e42")
greyCenterParaStyle = ParagraphStyle('greyParaStyle',parent=style['BodyText'],textColor="#443e42",alignment=TA_CENTER)
# offCourseStyle = ParagraphStyle('offCourseStyle',parent=style['BodyText'],textColor="white",alignment=TA_CENTER,backColor="#d83110")
# progressStyle = ParagraphStyle('progressStyle',parent=style['BodyText'],textColor="#443e42",alignment=TA_CENTER,backColor="#f39000")
# onCourseStyle = ParagraphStyle('onCourseStyle',parent=style['BodyText'],textColor="#443e42",alignment=TA_CENTER,backColor="#d8da00")
offCourseStyle = ParagraphStyle('offCourseStyle',parent=style['BodyText'],textColor="#d83110",alignment=TA_CENTER)
progressStyle = ParagraphStyle('progressStyle',parent=style['BodyText'],textColor="#f39000",alignment=TA_CENTER)
onCourseStyle = ParagraphStyle('onCourseStyle',parent=style['BodyText'],textColor="#d8da00",alignment=TA_CENTER)

def condStyle(progress):
    if progress=="On course":
        return onCourseStyle
    elif progress=="No progress or worsening":
        return offCourseStyle
    elif progress=="Some progress":
        return progressStyle
    elif progress=="Off course":
        return offCourseStyle
    else:
        return greyCenterParaStyle
        
#Read a CSV to make this data?
dataDictionary = {"Mozambique":{}}
dataDictionary["Mozambique"]["country"] = "Mozambique"
dataDictionary["Mozambique"]["table1"] = [["Gini index score*",Paragraph("Gini index rank**",style=whiteParaStyle),"Year"],[51,125,2011]]
dataDictionary["Mozambique"]["table2"] = [
    ["Population (thousands)",format(12428,",d"),2015]
    ,["Under-5 population (thousands)",format(1935,",d"),2015]
    ,["Urban (%)",format(20,",d"),2015]
    ,[">65 years (%)",format(5,",d"),2015]
    ]
dataDictionary["Mozambique"]["table3"] = [
    ["Number of children under 5 affected (thousands)","",""]
    ,[Paragraph("Stunting<super>1</super>",style=greyParaStyle),format(733,",d"),2015]
    ,[Paragraph("Wasting<super>1</super>",style=greyParaStyle),format(43,",d"),2015]
    ,[Paragraph("Overweight<super>1</super>",style=greyParaStyle),format(149,",d"),2015]
    ,["% of children under 5 affected","",""]
    ,[Paragraph("Wasting<super>1</super>",style=greyParaStyle),format(2,"d"),2015]
    ,[Paragraph("Severe wasting<super>1</super>",style=greyParaStyle),format(1,"d"),2015]
    ,[Paragraph("Overweight<super>1</super>",style=greyParaStyle),format(8,"d"),2015]
    ,[Paragraph("Low birth weight<super>2</super>",style=greyParaStyle),format(7,"d"),2015]
    ]
dataDictionary["Mozambique"]["table4"] = [
    [Paragraph("Adolescent overweight<super>1</super>",style=greyParaStyle),"NA","NA"]
    ,[Paragraph("Adolescent obesity<super>1</super>",style=greyParaStyle),"NA","NA"]
    ,[Paragraph("Women of reproductive age, thinness<super>2</super>",style=greyParaStyle),format(5,"d"),2010]
    ,[Paragraph("Women of reproductive age, short stature<super>2</super>",style=greyParaStyle),format(2,"d"),2010]
]
dataDictionary["Mozambique"]["table5"] = [
    [Paragraph("Women of reproductive age with anaemia<super>1</super>",style=greyParaStyle),"",""]
    ,["Total population affected (thousands of people)",format(467,",d"),2011]
    ,["Total population affected (%)",format(17,"d"),2011]
    ,[Paragraph(u"Vitamin A deficiency in children 6\u201359 months old (%)<super>2</super>",style=greyParaStyle),format(39,"d"),2013]
    ,[Paragraph(u"Population classification of iodine nutrition (age group 5\u201319 years old)<super>3</super>",style=greyParaStyle),Paragraph("Risk of iodine-induced hyperthyroidism (IIH) within 5-10 years following introduction of iodized salt in susceptible groups)",style=greyParaStyle),1996]
]
dataDictionary["Mozambique"]["table6"] = [
    [
        Paragraph("<b>Under-5 stunting</b>",style=whiteParaStyle)
        ,Paragraph("<b>Under-5 wasting</b>",style=whiteParaStyle)
        ,Paragraph("<b>Under-5 overweight</b>",style=whiteParaStyle)
        ,Paragraph("<b>WRA anaemia</b>",style=whiteParaStyle)
        ,Paragraph("<b>EBF</b>",style=whiteParaStyle)
     ]
    ,["Off course, some progress","On course","Off course, no progress","Off course","On course"]
]
dataDictionary["Mozambique"]["table6a"] = [
    [
        Paragraph("<b>Adult female obesity</b>",style=whiteParaStyle)
        ,Paragraph("<b>Adult male obesity</b>",style=whiteParaStyle)
        ,Paragraph("<b>Adult female diabetes</b>",style=whiteParaStyle)
        ,Paragraph("<b>Adult male diabetes</b>",style=whiteParaStyle)
     ]
    ,["Off course, some progress","On course","Off course, no progress","Off course"]
]
dataDictionary["Mozambique"]["table7"] = [
    [Paragraph("Severe acute malnutrition, geographic coverage<super>1</super>",style=greyParaStyle),"9","2012"]
    ,[Paragraph("Vitamin A supplementation, full coverage<super>2</super>",style=greyParaStyle),"65","2013"]
    ,[Paragraph("Children under 5 with diarrhoea receiving ORS<super>2</super>",style=greyParaStyle),"44","2011"]
    ,[Paragraph("Immunisation coverage, DTP3<super>3</super>",style=greyParaStyle),"78","2016"]
    ,[Paragraph("Iodised salt consumption<super>2</super>",style=greyParaStyle),"87","2006"]
]
dataDictionary["Mozambique"]["table8"] = [
    ["Minimum acceptable diet","6","2011"]
    ,["Minimum dietary diversity","13","2011"]
]
dataDictionary["Mozambique"]["table9"] = [
    [Paragraph("Early childbearing: births by age 18 (%)<super>1</super>",style=greyParaStyle),"33","2011"]
    ,[Paragraph("Gender Inequality Index (score*)<super>2</super>",style=greyParaStyle),"0.529","2013"]
    ,[Paragraph("Gender Inequality Index (country rank)<super>2</super>",style=greyParaStyle),"155","2013"]
]
dataDictionary["Mozambique"]["table10"] = [
    ["Physicians","0.117","2005"]
    ,["Nurses and midwives","1.306","2005"]
    ,["Community health workers","0.188","2005"]
]
dataDictionary["Mozambique"]["table11"] = [
    [Paragraph("National implementation of the International Code of Marketing of Breast-milk Substitutes<super>1</super>",style=greyParaStyle),"Law","2014"]
    ,[Paragraph("Extent of constitutional right to food<super>2</super>",style=greyParaStyle),"High","2003"]
    ,[Paragraph("Maternity Protection Convention 183<super>3</super>",style=greyParaStyle),"No","2011"]
    ,[Paragraph("Wheat fortification legislation<super>4</super>",style=greyParaStyle),"Mandatory","2015"]
    ,[Paragraph("Undernutrition mentioned in national development plans and economic growth strategies<super>5</super>",style=greyParaStyle),"Rank: 39/126","2010-2015"]
    ,[Paragraph("Overnutrition mentioned in national development plans and economic growth strategies<super>5</super>",style=greyParaStyle),"Rank: 57/116","2010-2015"]
]
dataDictionary["Mozambique"]["table12"] = [
    [Paragraph("All major NCDs",style=greyParaStyle),Paragraph("Available, partially implemented",style=greyParaStyle),"2010"]
]

dataDictionary["Afghanistan"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Albania"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Algeria"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Andorra"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Angola"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Antigua and Barbuda"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Argentina"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Armenia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Australia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Austria"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Azerbaijan"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Bahamas"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Bahrain"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Bangladesh"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Barbados"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Belarus"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Belgium"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Belize"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Benin"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Bhutan"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Bolivia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Bosnia and Herzegovina"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Botswana"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Brazil"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Brunei Darussalam"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Bulgaria"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Burkina Faso"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Burundi"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Cambodia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Cameroon"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Canada"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Cape Verde"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Central African Republic"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Chad"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Chile"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["China"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Colombia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Comoros"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Congo (Republic of the)"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Costa Rica"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Cote d'Ivoire"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Croatia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Cuba"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Cyprus"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Czech Republic"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Democratic People's Republic of Korea"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Democratic Republic of the Congo"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Denmark"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Djibouti"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Dominica"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Dominican Republic"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Ecuador"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Egypt"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["El Salvador"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Equatorial Guinea"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Eritrea"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Estonia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Ethiopia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Fiji"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Finland"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["France"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Gabon"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Gambia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Georgia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Germany"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Ghana"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Greece"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Grenada"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Guatemala"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Guinea"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Guinea-Bissau"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Guyana"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Haiti"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Honduras"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Hungary"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Iceland"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["India"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Indonesia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Iran"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Iraq"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Ireland"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Israel"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Italy"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Jamaica"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Japan"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Jordan"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Kazakhstan"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Kenya"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Kiribati"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Kuwait"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Kyrgyzstan"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Lao People's Democratic Republic"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Latvia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Lebanon"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Lesotho"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Liberia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Libya"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Liechtenstein"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Lithuania"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Luxembourg"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Madagascar"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Malawi"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Malaysia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Maldives"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Mali"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Malta"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Marshall Islands"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Mauritania"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Mauritius"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Mexico"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Micronesia (Federated States of)"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Monaco"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Mongolia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Montenegro"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Morocco"] = copy.deepcopy(dataDictionary["Mozambique"])
# dataDictionary["Mozambique"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Myanmar"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Namibia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Nauru"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Nepal"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Netherlands"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["New Zealand"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Nicaragua"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Niger"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Nigeria"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Norway"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Oman"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Pakistan"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Palau"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Panama"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Papua New Guinea"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Paraguay"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Peru"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Philippines"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Poland"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Portugal"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Qatar"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Republic of Korea"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Republic of Moldova"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Romania"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Russian Federation"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Rwanda"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Saint Kitts and Nevis"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Saint Lucia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Saint Vincent and the Grenadines"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Samoa"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["San Marino"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Sao Tome and Principe"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Saudi Arabia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Senegal"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Serbia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Seychelles"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Sierra Leone"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Singapore"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Slovakia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Slovenia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Solomon Islands"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Somalia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["South Africa"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["South Sudan"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Spain"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Sri Lanka"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Sudan"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Suriname"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Swaziland"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Sweden"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Switzerland"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Syria"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Tajikistan"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Thailand"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["The former Yugoslav Republic of Macedonia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Timor-Leste"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Togo"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Tonga"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Trinidad and Tobago"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Tunisia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Turkey"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Turkmenistan"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Tuvalu"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Uganda"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Ukraine"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["United Arab Emirates"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["United Kingdom"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["United Republic of Tanzania"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["United States of America"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Uruguay"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Uzbekistan"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Vanuatu"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Venezuela"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Viet Nam"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Yemen"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Zambia"] = copy.deepcopy(dataDictionary["Mozambique"])
dataDictionary["Zimbabwe"] = copy.deepcopy(dataDictionary["Mozambique"])

def replaceDash(x):
    x = str(x)
    y = re.sub(r"((?:^|[^{])\d+)-(\d+[^}])",u"\\1\u2013\\2", x)
    return y
missingVals = [" ",".","","Insufficient data to make assessment"]
def safeFormat(x,commas=False,precision=0):
    if pd.isnull(x):
        return "NA"
    elif x in missingVals:
        return "NA"
    else:
        if not isinstance(x,numbers.Number):
            return replaceDash(x)
        if precision == 0:
            x = int(round(x,precision))
        else:
            x = round(x,precision)
        if commas:
            return format(x,",")
        else:
            return x

dat = pd.read_csv("data.csv")
for country in dataDictionary.keys():
    row = dat.loc[(dat.country==country)].iloc[0]
    dataDictionary[country]["country"] = country
    
    # dataDictionary["Mozambique"]["table1"] = [["Gini index score*",Paragraph("Gini index rank<super>†</super>",style=whiteParaStyle),"Year"],[51,125,2011]]
    dataDictionary[country]["table1"][1] = [safeFormat(row["value_gini"]),safeFormat(row["rank_gini"]),safeFormat(row["year_gini"])]
    # dataDictionary["Mozambique"]["table2"] = [
    #     ["Population (000)",format(12428,",d"),2015]
    #     ,["Under-5 population (000)",format(1935,",d"),2015]
    #     ,["Urban (%)",format(20,",d"),2015]
    #     ,[">65 years (%)",format(5,",d"),2015]
    #     ]
    dataDictionary[country]["table2"][0][1] = safeFormat(row["totalpop2017"],True)
    dataDictionary[country]["table2"][0][2] = safeFormat(2017)
    dataDictionary[country]["table2"][1][1] = safeFormat(row["under5pop"],True)
    dataDictionary[country]["table2"][1][2] = safeFormat(2017)
    dataDictionary[country]["table2"][2][1] = safeFormat(row["urbanpop"])
    dataDictionary[country]["table2"][2][2] = safeFormat(2017)
    dataDictionary[country]["table2"][3][1] = safeFormat(row["over65pop"],True)
    dataDictionary[country]["table2"][3][2] = safeFormat(2017)
    # dataDictionary["Mozambique"]["table3"] = [
    #     ["Number of children under 5 affected (000)","",""]
    #     ,[Paragraph("Stunting<super>a</super>",style=greyParaStyle),format(733,",d"),2015]
    #     ,[Paragraph("Wasting<super>a</super>",style=greyParaStyle),format(43,",d"),2015]
    #     ,[Paragraph("Overweight<super>a</super>",style=greyParaStyle),format(149,",d"),2015]
    #     ,["Percentage of children under 5 affected","",""]
    #     ,[Paragraph("Wasting<super>a</super>",style=greyParaStyle),format(2,"d"),2015]
    #     ,[Paragraph("Severe wasting<super>a</super>",style=greyParaStyle),format(1,"d"),2015]
    #     ,[Paragraph("Overweight<super>a</super>",style=greyParaStyle),format(8,"d"),2015]
    #     ,[Paragraph("Low birth weight<super>b</super>",style=greyParaStyle),format(7,"d"),2015]
    #     ]
    dataDictionary[country]["table3"][1][1] = safeFormat(row["number_stunting_current"],True)
    dataDictionary[country]["table3"][1][2] = safeFormat(row["year_stunting_current"])
    dataDictionary[country]["table3"][2][1] = safeFormat(row["number_wasting"],True)
    dataDictionary[country]["table3"][2][2] = safeFormat(row["year_wasting"])
    dataDictionary[country]["table3"][3][1] = safeFormat(row["number_u5overweight"],True)
    dataDictionary[country]["table3"][3][2] = safeFormat(row["year_u5overweight"])
    dataDictionary[country]["table3"][5][1] = safeFormat(row["prev_wasting"])
    dataDictionary[country]["table3"][5][2] = safeFormat(row["year_wasting"])
    dataDictionary[country]["table3"][6][1] = safeFormat(row["prev_sev_wasting"])
    dataDictionary[country]["table3"][6][2] = safeFormat(row["year_sev_wasting"])
    dataDictionary[country]["table3"][7][1] = safeFormat(row["prev_u5overweight"])
    dataDictionary[country]["table3"][7][2] = safeFormat(row["year_u5overweight"])
    dataDictionary[country]["table3"][8][1] = safeFormat(row["LBW"])
    dataDictionary[country]["table3"][8][2] = safeFormat(row["year_lbw"])
    # dataDictionary["Mozambique"]["table4"] = [
    #     [Paragraph("Adolescent overweight<super>a</super>",style=greyParaStyle),"NA","NA"]
    #     ,[Paragraph("Adolescent obesity<super>a</super>",style=greyParaStyle),"NA","NA"]
    #     ,[Paragraph("Women of reproductive age, thinness<super>b</super>",style=greyParaStyle),format(5,"d"),2010]
    #     ,[Paragraph("Women of reproductive age, short stature<super>b</super>",style=greyParaStyle),format(2,"d"),2010]
    # ]
    dataDictionary[country]["table4"][0][1] = safeFormat(row["AdolOW"])
    dataDictionary[country]["table4"][0][2] = safeFormat(row["year_adolOWOB"])
    dataDictionary[country]["table4"][1][1] = safeFormat(row["AdolOB"])
    dataDictionary[country]["table4"][1][2] = safeFormat(row["year_adolOWOB"])
    dataDictionary[country]["table4"][2][1] = safeFormat(row["prev_BMI185"])
    dataDictionary[country]["table4"][2][2] = safeFormat(row["year_BMI185"])
    dataDictionary[country]["table4"][3][1] = safeFormat(row["prev_height145"])
    dataDictionary[country]["table4"][3][2] = safeFormat(row["year_height145"])
    # dataDictionary["Mozambique"]["table5"] = [
    #     [Paragraph("Women of reproductive age with anemia<super>a</super>",style=greyParaStyle),"",""]
    #     ,["Total population affected (000)",format(467,",d"),2011]
    #     ,["Total population affected (%)",format(17,"d"),2011]
    #     ,[Paragraph("Vitamin A deficiency in children 6-59 months old (%)<super>b</super>",style=greyParaStyle),format(39,"d"),2013]
    #     ,[Paragraph("Population classification of iodine nutrition (age group 5-19)<super>c</super>",style=greyParaStyle),Paragraph("Risk of iodine-induced hyperthyroidism (IIH) within 5-10 years following introduction of iodized salt in susceptible groups)",style=greyParaStyle),1996]
    # ]
    dataDictionary[country]["table5"][1][1] = safeFormat(row["WRAanaemia_NUMBER"],True)
    dataDictionary[country]["table5"][1][2] = safeFormat(row["year_WRAanaemia"])
    dataDictionary[country]["table5"][2][1] = safeFormat(row["WRAanaemia_RATE"])
    dataDictionary[country]["table5"][2][2] = safeFormat(row["year_WRAanaemia"])
    dataDictionary[country]["table5"][3][1] = safeFormat(row["prevalence_vita"])
    dataDictionary[country]["table5"][3][2] = safeFormat(row["year_vitA_def"])
    dataDictionary[country]["table5"][4][1] = Paragraph(safeFormat(row["Class_IodineNutrition"]),style=greyCenterParaStyle)
    dataDictionary[country]["table5"][4][2] = safeFormat(row["year_IodineNutrition"])
    # dataDictionary["Mozambique"]["table6"] = [
    #     [
    #         Paragraph("<b>Under-5 stunting, 2015<super>a</super></b>",style=whiteParaStyle)
    #         ,Paragraph("<b>Under-5 wasting, 2015<super>b</super></b>",style=whiteParaStyle)
    #         ,Paragraph("<b>Under-5 overweight, 2015<super>a</super></b>",style=whiteParaStyle)
    #         ,Paragraph("<b>WRA Anemia, 2011<super>b</super></b>",style=whiteParaStyle)
    #         ,Paragraph("<b>EBF, 2014-2015<super>a</super></b>",style=whiteParaStyle)
    #      ]
    #     ,["Off course, some progress","On course","Off course, no progress","Off course","On course"]
    # ]
    # dataDictionary[country]["table6"][0] = [
    #     Paragraph("<b>Under-5 stunting, %s<super>1</super></b>" % safeFormat(row["year_stunting_current"]),style=whiteParaStyle)
    #     ,Paragraph("<b>Under-5 wasting, %s<super>2</super></b>" % safeFormat(row["year_wasting"]),style=whiteParaStyle)
    #     ,Paragraph("<b>Under-5 overweight, %s<super>1</super></b>" % safeFormat(row["year_u5overweight"]),style=whiteParaStyle)
    #     ,Paragraph("<b>WRA anaemia, %s<super>2</super></b>" % safeFormat(row["year_WRAanaemia"]),style=whiteParaStyle)
    #     ,Paragraph("<b>EBF, %s<super>1</super></b>" % safeFormat(row["year_ebf_current"]),style=whiteParaStyle)
    # ]
    dataDictionary[country]["table6"][1] = [
        Paragraph(safeFormat(row["stunting_progress"]),style=condStyle(row["stunting_progress"]))
        ,Paragraph(safeFormat(row["wasting_progress"]),style=condStyle(row["wasting_progress"]))
        ,Paragraph(safeFormat(row["u5overweight_progress"]),style=condStyle(row["u5overweight_progress"]))
        ,Paragraph(safeFormat(row["progress_WRAanaemia"]),style=condStyle(row["progress_WRAanaemia"]))
        ,Paragraph(safeFormat(row["EBF_progress"]),style=condStyle(row["EBF_progress"]))
        ]
    # dataDictionary["Mozambique"]["table6a"] = [
    #     [
    #         Paragraph("<b>Adult female obesity, 2015<super>a</super></b>",style=whiteParaStyle)
    #         ,Paragraph("<b>Adult male obesity, 2015<super>a</super></b>",style=whiteParaStyle)
    #         ,Paragraph("<b>Adult female diabetes, 2015<super>a</super></b>",style=whiteParaStyle)
    #         ,Paragraph("<b>Adult male diabetes, 2015<super>a</super></b>",style=whiteParaStyle)
    #      ]
    #     ,["Off course, some progress","On course","Off course, no progress","Off course"]
    # ]
    # dataDictionary[country]["table6a"][0] = [
    #     Paragraph("<b>Adult female obesity, %s<super>1</super></b>" % safeFormat(2017),style=whiteParaStyle)
    #     ,Paragraph("<b>Adult male obesity, %s<super>1</super></b>" % safeFormat(2017),style=whiteParaStyle)
    #     ,Paragraph("<b>Adult female diabetes, %s<super>1</super></b>" % safeFormat(2017),style=whiteParaStyle)
    #     ,Paragraph("<b>Adult male diabetes, %s<super>1</super></b>" % safeFormat(2017),style=whiteParaStyle)
    # ]
    dataDictionary[country]["table6a"][1] = [
        Paragraph(safeFormat(row["ob_female_progress"]),style=condStyle(row["ob_female_progress"]))
        ,Paragraph(safeFormat(row["ob_male_progress"]),style=condStyle(row["ob_male_progress"]))
        ,Paragraph(safeFormat(row["dm_female_progress"]),style=condStyle(row["dm_female_progress"]))
        ,Paragraph(safeFormat(row["dm_male_progress"]),style=condStyle(row["dm_male_progress"]))
        ]
    # dataDictionary["Mozambique"]["table7"] = [
    #     [Paragraph("Severe acute malnutrition, geographic coverage<super>a</super>",style=greyParaStyle),"9","2012"]
    #     ,[Paragraph("Vitamin A supplementation, full coverage<super>b</super>",style=greyParaStyle),"65","2013"]
    #     ,[Paragraph("Children under 5 with diarrhea receiving ORS<super>b</super>",style=greyParaStyle),"44","2011"]
    #     ,[Paragraph("Immunization coverage, DTP3<super>b</super>",style=greyParaStyle),"78","2013"]
    #     ,[Paragraph("Iodized salt consumption<super>b</super>",style=greyParaStyle),"87","2006"]
    # ]
    dataDictionary[country]["table7"][0][1] = safeFormat(row["SAMcoverage_rate"])
    dataDictionary[country]["table7"][0][2] = safeFormat(row["year_SAM"])
    dataDictionary[country]["table7"][1][1] = safeFormat(row["vitA_supp"])
    dataDictionary[country]["table7"][1][2] = safeFormat(row["year_vitA_supp"])
    dataDictionary[country]["table7"][2][1] = safeFormat(row["ors"])
    dataDictionary[country]["table7"][2][2] = safeFormat(row["yr_ors"])
    dataDictionary[country]["table7"][3][1] = safeFormat(row["DTP3"])
    dataDictionary[country]["table7"][3][2] = safeFormat(row["year_DTP"])
    dataDictionary[country]["table7"][4][1] = safeFormat(row["householdconsumingsalt"])
    dataDictionary[country]["table7"][4][2] = safeFormat(row["year_Iodised"])
    # dataDictionary["Mozambique"]["table8"] = [
    #     ["Minimum acceptable diet","6","2011"]
    #     ,["Minimum dietary diversity","13","2011"]
    # ]
    dataDictionary[country]["table8"][0][1] = safeFormat(row["mad"])
    dataDictionary[country]["table8"][0][2] = safeFormat(row["yr_mad"])
    dataDictionary[country]["table8"][1][1] = safeFormat(row["mdd"])
    dataDictionary[country]["table8"][1][2] = safeFormat(row["yr_mdd"])
    # dataDictionary["Mozambique"]["table9"] = [
    #     [Paragraph("Early childbearing: births by age 18 (%)<super>a</super>",style=greyParaStyle),"33","2011"]
    #     ,[Paragraph("Gender Inequality Index (score*)<super>b</super>",style=greyParaStyle),"0.529","2013"]
    #     ,[Paragraph("Gender Inequality Index (country rank)<super>b</super>",style=greyParaStyle),"155","2013"]
    # ]
    dataDictionary[country]["table9"][0][1] = safeFormat(row["earlychild"])
    dataDictionary[country]["table9"][0][2] = safeFormat(row["year_early_child"])
    dataDictionary[country]["table9"][1][1] = safeFormat(row["index_genderinequality"],False,3)
    dataDictionary[country]["table9"][1][2] = safeFormat(row["year_genderinequality"])
    dataDictionary[country]["table9"][2][1] = safeFormat(row["rank_genderinequality"])
    dataDictionary[country]["table9"][2][2] = safeFormat(row["year_genderinequality"])
    # dataDictionary["Mozambique"]["table10"] = [
    #     ["Physicians","0.117","2005"]
    #     ,["Nurses and midwives","1.306","2005"]
    #     ,["Community health workers","0.188","2005"]
    # ]
    dataDictionary[country]["table10"][0][1] = safeFormat(row["valuephysician"],False,3)
    dataDictionary[country]["table10"][0][2] = safeFormat(row["yearphysician"])
    dataDictionary[country]["table10"][1][1] = safeFormat(row["valuenurse"],False,3)
    dataDictionary[country]["table10"][1][2] = safeFormat(row["yearnurse"])
    dataDictionary[country]["table10"][2][1] = safeFormat(row["valuehealthworker"],False,3)
    dataDictionary[country]["table10"][2][2] = safeFormat(row["yearhealthworker"])
    # dataDictionary["Mozambique"]["table11"] = [
    #     [Paragraph("National implementation of the International Code of Marketing of Breast-milk Substitutes<super>a</super>",style=greyParaStyle),"Law","2014"]
    #     ,[Paragraph("Extent of constitutional right to food<super>b</super>",style=greyParaStyle),"High","2003"]
    #     ,[Paragraph("Maternity protection (Convention 183)<super>c</super>",style=greyParaStyle),"No","2011"]
    #     ,[Paragraph("Wheat fortification legislation<super>d</super>",style=greyParaStyle),"Mandatory","2015"]
    #     ,[Paragraph("Undernutrition mentioned in national development plans and economic growth strategies<super>e</super>",style=greyParaStyle),"Rank: 39/126","2010-2015"]
    #     ,[Paragraph("Overnutrition mentioned in national development plans and economic growth strategies<super>e</super>",style=greyParaStyle),"Rank: 57/116","2010-2015"]
    # ]
    dataDictionary[country]["table11"][0][1] = Paragraph(safeFormat(row["Code_breastfeeding"]),style=greyCenterParaStyle)
    dataDictionary[country]["table11"][0][2] = safeFormat(row["year_code_bf"])
    dataDictionary[country]["table11"][1][1] = Paragraph(safeFormat(row["RTF_level1"]),style=greyCenterParaStyle)
    dataDictionary[country]["table11"][1][2] = safeFormat(row["year_RTF"])
    dataDictionary[country]["table11"][2][1] = Paragraph(safeFormat(row["cat_maternityprotection"]),style=greyCenterParaStyle)
    dataDictionary[country]["table11"][2][2] = safeFormat(row["year_maternityprotection"])
    dataDictionary[country]["table11"][3][1] = Paragraph(safeFormat(row["Fortification"]),style=greyCenterParaStyle)
    dataDictionary[country]["table11"][3][2] = safeFormat(row["year_FFI"])
    dataDictionary[country]["table11"][4][1] = safeFormat(row["undernutritionrank_1to126"])
    dataDictionary[country]["table11"][4][2] = safeFormat(row["year_policyrank"])
    dataDictionary[country]["table11"][5][1] = safeFormat(row["overnutritionrank_1to116"])
    dataDictionary[country]["table11"][5][2] = safeFormat(row["year_policyrank"])
    # dataDictionary["Mozambique"]["table12"] = [
    #     ["All major NCDs",Paragraph("Available, partially implemented",style=greyParaStyle),"2010"]
    # ]
    dataDictionary[country]["table12"][0][1] = Paragraph(safeFormat(row["ncd_policy"]),style=greyCenterParaStyle)
    dataDictionary[country]["table12"][0][2] = safeFormat(row["year_ncdpolicy"])

tableStyles = {}
tableStyles["table1"] = [
    ('TEXTCOLOR',(0,0),(-1,-1),"white")
    ,('BACKGROUND',(0,0),(2,0),"#7b1059")
    ,('FONTNAME',(0,1),(2,1),"Arial-Bold")
    # ,('FONTNAME',(0,1),(2,1),"Arial")
    ,('BACKGROUND',(0,1),(2,1),"#c79ec5")
    # ,('GRID',(0,0),(-1,-1),1,"white")
    ,('LINEAFTER',(0,0),(1,1),1,"white")
    ,('ALIGN',(0,0),(-1,-1),"CENTER")
    ,('VALIGN',(0,0),(-1,-1),"MIDDLE")
    ]
tableStyles["table2"] = [
    ('BACKGROUND',(0,1),(-1,1),"#fef5e7")
    ,('BACKGROUND',(0,3),(-1,3),"#fef5e7")
    ,('ALIGN',(0,0),(0,-1),"LEFT")
    ,('ALIGN',(1,0),(2,-1),"CENTER")
    ,('VALIGN',(0,0),(-1,-1),"MIDDLE")
    ,('BOX',(1,0),(1,-1),1,"#f79c2a")
    ,('LINEABOVE',(0,0),(-1,0),1,"#f79c2a")
    ,('LINEBELOW',(0,-1),(-1,-1),1,"#f79c2a")
    ,('TEXTCOLOR',(0,0),(-1,-1),"#443e42")
    ]
tableStyles["table3"] = [
    ('BACKGROUND',(0,1),(-1,1),"#fef5e7")
    ,('BACKGROUND',(0,3),(-1,3),"#fef5e7")
    ,('BACKGROUND',(0,5),(-1,5),"#fef5e7")
    ,('BACKGROUND',(0,7),(-1,7),"#fef5e7")
    ,('ALIGN',(0,0),(0,-1),"LEFT")
    ,('ALIGN',(1,0),(2,-1),"CENTER")
    ,('VALIGN',(0,0),(-1,-1),"MIDDLE")
    ,('BOX',(1,1),(1,3),1,"#f79c2a")
    ,('BOX',(1,5),(1,-1),1,"#f79c2a")
    ,('LINEABOVE',(0,0),(-1,0),1,"#f79c2a")
    ,('LINEABOVE',(0,1),(-1,1),1,"#f79c2a")
    ,('LINEABOVE',(0,4),(-1,4),1,"#f79c2a")
    ,('LINEABOVE',(0,5),(-1,5),1,"#f79c2a")
    ,('LINEBELOW',(0,-1),(-1,-1),1,"#f79c2a")
    ,('SPAN',(0,0),(-1,0))
    # ,('FONTNAME',(0,0),(-1,0),"Arial-Bold")
    ,('SPAN',(0,4),(-1,4))
    # ,('FONTNAME',(0,4),(-1,4),"Arial-Bold")
    ,('TEXTCOLOR',(0,0),(-1,-1),"#443e42")
    ]
tableStyles["table4"] = tableStyles["table2"]
tableStyles["table5"] = [
    ('LINEABOVE',(0,0),(-1,0),1,"#f79c2a")
    ,('LINEABOVE',(0,1),(-1,1),1,"#f79c2a")
    ,('LINEABOVE',(0,3),(-1,3),1,"#f79c2a")
    ,('LINEBELOW',(0,4),(-1,4),1,"#f79c2a")
    ,('SPAN',(0,0),(-1,0))
    ,('LINEAFTER',(0,1),(1,2),.5,"#fbcd99")
    ,('LINEBELOW',(0,1),(-1,1),.5,"#fbcd99")
    ,('LINEAFTER',(0,3),(1,-1),1,"#fbcd99")
    ,('LINEABOVE',(0,-1),(-1,-1),1,"#fbcd99")
    ,('VALIGN',(0,0),(-1,-1),"MIDDLE")
    ,('ALIGN',(0,0),(0,-1),"LEFT")
    ,('ALIGN',(1,0),(2,-1),"CENTER")
    ,('TEXTCOLOR',(0,0),(-1,-1),"#443e42")
]
tableStyles["table6"] = [
    ('TEXTCOLOR',(0,0),(-1,0),"white")
    # ,('BACKGROUND',(0,0),(-1,0),"#204d5e")
    # ,('BACKGROUND',(0,1),(-1,1),"white")
    # ,('GRID',(0,0),(-1,-1),1,"#386170")
    ,('ALIGN',(0,0),(-1,-1),"CENTER")
    ,('VALIGN',(0,0),(-1,-1),"MIDDLE")
    ,('TEXTCOLOR',(0,1),(-1,-1),"#443e42")
    ]
tableStyles["table6a"] = tableStyles["table6"]
tableStyles["table7"] = tableStyles["table2"]
tableStyles["table8"] = [
    ('BACKGROUND',(0,1),(-1,1),"#fef5e7")
    ,('ALIGN',(0,0),(0,-1),"LEFT")
    ,('ALIGN',(1,0),(2,-1),"CENTER")
    ,('VALIGN',(0,0),(-1,-1),"MIDDLE")
    ,('BOX',(1,0),(1,-1),1,"#f79c2a")
    ,('LINEABOVE',(0,0),(-1,0),1,"#f79c2a")
    ,('LINEBELOW',(0,-1),(-1,-1),1,"#f79c2a")
    ,('TEXTCOLOR',(0,0),(-1,-1),"#443e42")
    ]
tableStyles["table9"] = tableStyles["table8"]
tableStyles["table10"] = tableStyles["table8"]
tableStyles["table11"] = [
    ('BACKGROUND',(0,1),(-1,1),"#fef5e7")
    ,('BACKGROUND',(0,3),(-1,3),"#fef5e7")
    ,('BACKGROUND',(0,5),(-1,5),"#fef5e7")
    ,('ALIGN',(0,0),(0,-1),"LEFT")
    ,('ALIGN',(1,0),(2,-1),"CENTER")
    ,('VALIGN',(0,0),(-1,-1),"MIDDLE")
    ,('BOX',(1,0),(1,-1),1,"#f79c2a")
    ,('LINEABOVE',(0,0),(-1,0),1,"#f79c2a")
    ,('LINEBELOW',(0,-1),(-1,-1),1,"#f79c2a")
    ,('TEXTCOLOR',(0,0),(-1,-1),"#443e42")
    ]
tableStyles["table12"] = tableStyles["table8"]