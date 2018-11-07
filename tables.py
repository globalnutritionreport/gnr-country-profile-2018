#!/usr/bin/python
# -*- coding: utf-8 -*-

from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.platypus import Paragraph
from reportlab.lib.enums import TA_LEFT
import copy
import pandas as pd
import numbers
import re
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from reportlab.lib.fonts import addMapping

blue = "#3D5163"
grey = "#C7D4E0"

pdfmetrics.registerFont(TTFont('Arial', 'fonts/Arial.ttf'))
pdfmetrics.registerFont(TTFont('Arial-Bold', 'fonts/Arial-Bold.ttf'))
pdfmetrics.registerFont(TTFont('Arial-Italic', 'fonts/Arial-Italic.ttf'))
addMapping('Arial', 0, 0, 'Arial')
addMapping('Arial', 0, 1, 'Arial-Italic')
addMapping('Arial', 1, 0, 'Arial-Bold')

style = getSampleStyleSheet()
blueParaBold = ParagraphStyle('blueParaBold', parent=style['BodyText'], textColor=blue, alignment=TA_LEFT, fontName="Arial-Bold")
blueParaStyle = ParagraphStyle('blueParaStyle', parent=style['BodyText'], textColor=blue, alignment=TA_LEFT)

dataDictionary = {"Kenya": {}}
dataDictionary["Kenya"]["country"] = "Kenya"

dataDictionary["Kenya"]["table1"] = [
    [
        Paragraph("<b>Under-5 stunting</b>", style=blueParaBold),
        Paragraph("<b>Under-5 wasting</b>", style=blueParaBold),
        Paragraph("<b>Under-5 overweight</b>", style=blueParaBold),
        Paragraph("<b>WRA anaemia</b>", style=blueParaBold),
        Paragraph("<b>EBF</b>", style=blueParaBold)
     ],
    ["Off course, some progress", "On course", "Off course, no progress", "Off course", "On course"]
]
dataDictionary["Kenya"]["table1a"] = [
    [
        Paragraph("<b>Adult female obesity</b>", style=blueParaBold),
        Paragraph("<b>Adult male obesity</b>", style=blueParaBold),
        Paragraph("<b>Adult female diabetes</b>", style=blueParaBold),
        Paragraph("<b>Adult male diabetes</b>", style=blueParaBold)
     ],
    ["Off course, some progress", "On course", "Off course, no progress", "Off course"]
]

dataDictionary["Kenya"]["tablea"] = [[Paragraph("Gini index score<super>1</super>", style=blueParaBold), Paragraph("Gini index rank<super>2</super>", style=blueParaBold), "Year"], [51, 125, 2011]]
dataDictionary["Kenya"]["tableb"] = [
    ["Population (000)", format(12428, ",d"), 2015],
    ["Under-5 population (000)", format(1935, ",d"), 2015],
    ["Urban (%)", format(20, ",d"), 2015],
    [">65 years (%)", format(5, ",d"), 2015],
    ]

# dataDictionary["Kenya"] = copy.deepcopy(dataDictionary["Kenya"])


def replaceDash(x):
    x = str(x)
    y = re.sub(r"((?:^|[^{])\d+)-(\d+[^}])", u"\\1\u2013\\2", x)
    return y
missingVals = [" ", ".", "", "Insufficient data to make assessment"]


def safeFormat(x, commas=False, precision=0):
    if pd.isnull(x):
        return "NA"
    elif x in missingVals:
        return "NA"
    else:
        if not isinstance(x, numbers.Number):
            return replaceDash(x)
        if precision == 0:
            x = int(round(x, precision))
        else:
            x = round(x, precision)
        if commas:
            return format(x, ",")
        else:
            return x


def indicator(ctryDat, indicator):
    return ctryDat.loc[(ctryDat["indicator"] == indicator)].iloc[0]["value"]


def year(ctryDat, indicator):
    return ctryDat.loc[(ctryDat["indicator"] == indicator)].iloc[0]["year"]

dat = pd.read_csv("data.csv")
for country in dataDictionary.keys():
    ctryDat = dat.loc[(dat.country == country)]
    dataDictionary[country]["country"] = country

    dataDictionary[country]["table1"][1] = [
        safeFormat(indicator(ctryDat, "under_5_stunting_track")),
        safeFormat(indicator(ctryDat, "under_5_wasting_track")),
        safeFormat(indicator(ctryDat, "under_5_overweight_track")),
        safeFormat(indicator(ctryDat, "wra_anaemia_track")),
        safeFormat(indicator(ctryDat, "ebf_track")),
    ]

    dataDictionary[country]["table1a"][1] = [
        safeFormat(indicator(ctryDat, "adult_fem_obesity_track")),
        safeFormat(indicator(ctryDat, "adult_mal_obesity_track")),
        safeFormat(indicator(ctryDat, "adult_fem_diabetes_track")),
        safeFormat(indicator(ctryDat, "adult_mal_diabetes_track"))
    ]

    dataDictionary[country]["tablea"][1] = [
        safeFormat(indicator(ctryDat, "gini")),
        safeFormat(indicator(ctryDat, "gini_rank")),
        safeFormat(indicator(ctryDat, "gini_year"))
    ]

    dataDictionary[country]["tableb"][0][1] = safeFormat(indicator(ctryDat, "population"), True)
    dataDictionary[country]["tableb"][0][2] = safeFormat(year(ctryDat, "population"))
    dataDictionary[country]["tableb"][1][1] = safeFormat(indicator(ctryDat, "u5_pop"), True)
    dataDictionary[country]["tableb"][1][2] = safeFormat(year(ctryDat, "u5_pop"))
    dataDictionary[country]["tableb"][2][1] = safeFormat(indicator(ctryDat, "urban_percent"))
    dataDictionary[country]["tableb"][2][2] = safeFormat(year(ctryDat, "urban_percent"))
    dataDictionary[country]["tableb"][3][1] = safeFormat(indicator(ctryDat, "65_years"), True)
    dataDictionary[country]["tableb"][3][2] = safeFormat(year(ctryDat, "65_years"))

generic_style = [
    ('TEXTCOLOR', (0, 0), (-1, -1), blue),
    ('BACKGROUND', (0, 0), (-1, -1), "white"),
    ('LINEABOVE', (0, 0), (-1, 0), 1, blue),
    ('ALIGN', (0, 0), (-1, -1), "LEFT"),
    ('VALIGN', (0, 0), (-1, -1), "MIDDLE"),
    ('LINEBELOW', (0, -1), (-1, -1), 1, grey)
]

tableStyles = {}
tableStyles["table1"] = [
    ('TEXTCOLOR', (0, 0), (-1, -1), blue),
    ('BACKGROUND', (0, 0), (-1, -1), "transparent"),
    ('ALIGN', (0, 0), (-1, -1), "LEFT"),
    ('VALIGN', (0, 0), (-1, -1), "MIDDLE"),
    ('FONTNAME', (0, 0), (-1, 0), "Arial-Bold")
]
tableStyles["table1a"] = tableStyles["table1"]
tableStyles["tablea"] = generic_style + [
    ('FONTNAME', (0, 0), (-1, 0), "Arial-Bold"),
    ('LINEABOVE', (0, 1), (-1, 1), 1, blue)
]
tableStyles["tableb"] = generic_style + [
    ('LINEABOVE', (0, 1), (-1, 1), 1, grey),
    ('LINEABOVE', (0, 2), (-1, 2), 1, grey),
    ('LINEABOVE', (0, 3), (-1, 3), 1, grey),
    ('FONTNAME', (0, 0), (0, -1), "Arial-Bold")
]
