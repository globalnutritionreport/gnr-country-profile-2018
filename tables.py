#!/usr/bin/python
# -*- coding: utf-8 -*-

from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.platypus import Paragraph
from reportlab.lib.enums import TA_LEFT, TA_CENTER
import copy
import pandas as pd
import numbers
import re
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from reportlab.lib.fonts import addMapping

dark_orange = "#DE5D09"
orange = "#F39000"
light_orange = "#FCC97A"
dark_aqua = "#007495"
aqua = "#93CAC9"
aqua_light = "#B2D8D7"
aqua_extra_light = "#D1E7E5"
dark_grey = "#475C6D"
grey = "#A0ADBB"
light_grey = "#CFD9E5"

pdfmetrics.registerFont(TTFont('Averta', 'fonts/Averta-Regular.ttf'))
pdfmetrics.registerFont(TTFont('Averta-Bold', 'fonts/Averta-Bold.ttf'))
pdfmetrics.registerFont(TTFont('Averta-Italic', 'fonts/Averta-RegularItalic.ttf'))
addMapping('Averta', 0, 0, 'Averta')
addMapping('Averta', 0, 1, 'Averta-Italic')
addMapping('Averta', 1, 0, 'Averta-Bold')

style = getSampleStyleSheet()
dark_greyParaBold = ParagraphStyle('dark_greyParaBold', parent=style['BodyText'], textColor=dark_grey, alignment=TA_LEFT, fontName="Averta-Bold")
dark_greyParaBoldCenter = ParagraphStyle('dark_greyParaBold', parent=style['BodyText'], textColor=dark_grey, alignment=TA_CENTER, fontName="Averta-Bold")
dark_greyParaStyle = ParagraphStyle('dark_greyParaStyle', parent=style['BodyText'], textColor=dark_grey, alignment=TA_LEFT)
NACourseStyle = ParagraphStyle('offCourseStyle', parent=style['BodyText'], textColor=grey, alignment=TA_LEFT)
offCourseStyle = ParagraphStyle('offCourseStyle', parent=style['BodyText'], textColor=orange, alignment=TA_LEFT)
progressStyle = ParagraphStyle('progressStyle', parent=style['BodyText'], textColor=aqua, alignment=TA_LEFT)
onCourseStyle = ParagraphStyle('onCourseStyle', parent=style['BodyText'], textColor=dark_aqua, alignment=TA_LEFT)


def condStyle(progress):
    if progress == "On course":
        return onCourseStyle
    elif progress == "No progress or worsening":
        return offCourseStyle
    elif progress == "Some progress":
        return progressStyle
    else:
        return NACourseStyle

dataDictionary = {"Kenya": {}}
dataDictionary["Kenya"]["country"] = "Kenya"

dataDictionary["Kenya"]["table1"] = [
    [
        Paragraph("<b>Under-5 stunting</b>", style=dark_greyParaBold),
        Paragraph("<b>Under-5 wasting</b>", style=dark_greyParaBold),
        Paragraph("<b>Under-5 overweight</b>", style=dark_greyParaBold),
        Paragraph("<b>WRA anaemia</b>", style=dark_greyParaBold),
        Paragraph("<b>EBF</b>", style=dark_greyParaBold)
     ],
    ["Off course, some progress", "On course", "Off course, no progress", "Off course", "On course"]
]

dataDictionary["Kenya"]["table1a"] = [
    [
        Paragraph("<b>Adult female obesity</b>", style=dark_greyParaBold),
        Paragraph("<b>Adult male obesity</b>", style=dark_greyParaBold),
        Paragraph("<b>Adult female diabetes</b>", style=dark_greyParaBold),
        Paragraph("<b>Adult male diabetes</b>", style=dark_greyParaBold)
     ],
    ["Off course, some progress", "On course", "Off course, no progress", "Off course"]
]

dataDictionary["Kenya"]["table2"] = [
    [Paragraph("Gini index score<super>1</super>", style=dark_greyParaBold), Paragraph("Gini index rank<super>2</super>", style=dark_greyParaBold), "Year"], [51, 125, 2011]
]

dataDictionary["Kenya"]["table3"] = [
    ["Population (000)", format(12428, ",d"), 2015],
    ["Under-5 population (000)", format(1935, ",d"), 2015],
    ["Rural (%)", format(20, ",d"), 2015],
    [">65 years (000)", format(5, ",d"), 2015],
]

dataDictionary["Kenya"]["table4"] = [
    [Paragraph("Early childbearing: births by age 18 (%)<super>1</super>", style=dark_greyParaBold), "33", "2011"],
    [Paragraph("Gender Inequality Index (score*)<super>2</super>", style=dark_greyParaBold), "0.529", "2013"],
    [Paragraph("Gender Inequality Index (country rank)<super>2</super>", style=dark_greyParaBold), "155", "2013"]
]
dataDictionary["Kenya"]["table5"] = [
    ["Physicians", "0.117", "2005"],
    ["Nurses and midwives", "1.306", "2005"],
    ["Community health workers", "0.188", "2005"]
]

dataDictionary["Kenya"]["table6"] = [
    [Paragraph("Mandatory legislation for salt iodisation", style=dark_greyParaBold), "Yes"],
    [Paragraph(u"Sugar\u2013sweeted beverage tax", style=dark_greyParaBold), "Yes"],
    [Paragraph("Multisectoral comprehensive nutrition plan", style=dark_greyParaBold), "Yes"],
    [Paragraph("Food-based dietary guidelines", style=dark_greyParaBold), "Yes"],
]

dataDictionary["Kenya"]["table7"] = [
    [
        Paragraph("Stunting", style=dark_greyParaBoldCenter),
        Paragraph("Anaemia", style=dark_greyParaBoldCenter),
        Paragraph("Low birth weight", style=dark_greyParaBoldCenter),
        Paragraph("Child overweight", style=dark_greyParaBoldCenter),
        Paragraph("Exclusive breastfeeding", style=dark_greyParaBoldCenter),
        Paragraph("Wasting", style=dark_greyParaBoldCenter),
        Paragraph("Salt intake", style=dark_greyParaBoldCenter),
        Paragraph("Overweight adults and adolescents", style=dark_greyParaBoldCenter),
    ],
    ["Yes", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"]
]

dataDictionary["Kenya"]["table8"] = [
    [
        Paragraph("<b>Coverage/practice indicator</b>", style=dark_greyParaBold),
        Paragraph("<b>%</b>", style=dark_greyParaBold),
        Paragraph("<b>Male</b>", style=dark_greyParaBold),
        Paragraph("<b>Female</b>", style=dark_greyParaBold),
        Paragraph("<b>Year</b>", style=dark_greyParaBold)
     ],
    [u"Children 0\u201359 months with dirrhea who received zinc treatment", "8.1", "", "", "2014"],
    [u"Children 6\u201359 months who received vitamin A supplements in last 6 months", "71.7", "71.6", "71.9", "2014"],
    [u"Children 6\u201359 months given iron supplements in past 7 days", "2.7", "2.6", "2.7", "2014"],
    [Paragraph("Women with a birth in last five years who received iron and folic acid during their most recent pregnancy", style=dark_greyParaStyle), "69.4", "", "", "2014"],
    ["Household consumption of adequately iodised salt", "99.5", "", "", "2014"],
]

dat = pd.read_csv("data.csv")
country_names = dat.country.unique()
for country_name in country_names:
    dataDictionary[country_name] = copy.deepcopy(dataDictionary["Kenya"])


def replaceDash(x):
    x = str(x)
    y = re.sub(r"((?:^|[^{])\d+)-(\d+[^}])", u"\\1\u2013\\2", x)
    return y
missingVals = [" ", ".", "", "Insufficient data to make assessment"]


def safeFormat(x, commas=False, precision=0, percent=False):
    if pd.isnull(x):
        return "NA"
    elif x in missingVals:
        return "NA"
    else:
        if percent:
            try:
                x = float(x) * 100
            except ValueError:
                return replaceDash(x)
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


def indicator_disagg(ctryDat, indicator, disagg, disagg_value=None):
    if disagg_value:
        return ctryDat.loc[(ctryDat["indicator"] == indicator) & (ctryDat["disaggregation"] == disagg) & (ctryDat["disagg.value"] == disagg_value)].iloc[0]["value"]
    else:
        return ctryDat.loc[(ctryDat["indicator"] == indicator) & (ctryDat["disaggregation"] == disagg)].iloc[0]["value"]


def year(ctryDat, indicator):
    return ctryDat.loc[(ctryDat["indicator"] == indicator)].iloc[0]["year"]

for country in dataDictionary.keys():
    ctryDat = dat.loc[(dat.country == country)]
    dataDictionary[country]["country"] = country
    dataDictionary[country]["country_class"] = indicator(ctryDat, "country_class")
    dataDictionary[country]["burden_text"] = indicator(ctryDat, "burden_text")
    dataDictionary[country]["table1"][1] = [
        Paragraph(safeFormat(indicator(ctryDat, "under_5_stunting_track")), style=condStyle(indicator(ctryDat, "under_5_stunting_track"))),
        Paragraph(safeFormat(indicator(ctryDat, "under_5_wasting_track")), style=condStyle(indicator(ctryDat, "under_5_wasting_track"))),
        Paragraph(safeFormat(indicator(ctryDat, "under_5_overweight_track")), style=condStyle(indicator(ctryDat, "under_5_overweight_track"))),
        Paragraph(safeFormat(indicator(ctryDat, "wra_anaemia_track")), style=condStyle(indicator(ctryDat, "wra_anaemia_track"))),
        Paragraph(safeFormat(indicator(ctryDat, "ebf_track")), style=condStyle(indicator(ctryDat, "ebf_track"))),
    ]

    dataDictionary[country]["table1a"][1] = [
        Paragraph(safeFormat(indicator(ctryDat, "adult_fem_obesity_track")), style=condStyle(indicator(ctryDat, "adult_fem_obesity_track"))),
        Paragraph(safeFormat(indicator(ctryDat, "adult_mal_obesity_track")), style=condStyle(indicator(ctryDat, "adult_mal_obesity_track"))),
        Paragraph(safeFormat(indicator(ctryDat, "adult_fem_diabetes_track")), style=condStyle(indicator(ctryDat, "adult_fem_diabetes_track"))),
        Paragraph(safeFormat(indicator(ctryDat, "adult_mal_diabetes_track")), style=condStyle(indicator(ctryDat, "adult_mal_diabetes_track"))),
    ]

    dataDictionary[country]["table2"][1] = [
        safeFormat(indicator(ctryDat, "gini")),
        safeFormat(indicator(ctryDat, "gini_rank")),
        safeFormat(year(ctryDat, "gini"))
    ]

    dataDictionary[country]["table3"][0][1] = safeFormat(indicator(ctryDat, "population"), True)
    dataDictionary[country]["table3"][0][2] = safeFormat(year(ctryDat, "population"))
    dataDictionary[country]["table3"][1][1] = safeFormat(indicator(ctryDat, "u5_pop"), True)
    dataDictionary[country]["table3"][1][2] = safeFormat(year(ctryDat, "u5_pop"))
    dataDictionary[country]["table3"][2][1] = safeFormat(indicator(ctryDat, "rural_percent"))
    dataDictionary[country]["table3"][2][2] = safeFormat(year(ctryDat, "rural_percent"))
    dataDictionary[country]["table3"][3][1] = safeFormat(indicator(ctryDat, "65_years"), True)
    dataDictionary[country]["table3"][3][2] = safeFormat(year(ctryDat, "65_years"))

    dataDictionary[country]["table4"][0][1] = safeFormat(indicator(ctryDat, "early_childbearing_prev"))
    dataDictionary[country]["table4"][0][2] = safeFormat(year(ctryDat, "early_childbearing_prev"))
    dataDictionary[country]["table4"][1][1] = safeFormat(indicator(ctryDat, "gender_inequality_score"), False, 3)
    dataDictionary[country]["table4"][1][2] = safeFormat(year(ctryDat, "gender_inequality_score"))
    dataDictionary[country]["table4"][2][1] = safeFormat(indicator(ctryDat, "gender_inequality_rank"))
    dataDictionary[country]["table4"][2][2] = safeFormat(year(ctryDat, "gender_inequality_rank"))

    dataDictionary[country]["table5"][0][1] = safeFormat(indicator(ctryDat, "physicians"), False, 3)
    dataDictionary[country]["table5"][0][2] = safeFormat(year(ctryDat, "physicians"))
    dataDictionary[country]["table5"][1][1] = safeFormat(indicator(ctryDat, "nurses_and_midwives"), False, 3)
    dataDictionary[country]["table5"][1][2] = safeFormat(year(ctryDat, "nurses_and_midwives"))
    dataDictionary[country]["table5"][2][1] = safeFormat(indicator(ctryDat, "community_health_workers"), False, 3)
    dataDictionary[country]["table5"][2][2] = safeFormat(year(ctryDat, "community_health_workers"))

    dataDictionary[country]["table6"][0][1] = safeFormat(indicator(ctryDat, "salt_leg"))
    dataDictionary[country]["table6"][1][1] = safeFormat(indicator(ctryDat, "sugar_tax"))
    dataDictionary[country]["table6"][2][1] = safeFormat(indicator(ctryDat, "multi_sec"))
    dataDictionary[country]["table6"][3][1] = safeFormat(indicator(ctryDat, "fbdg"))

    dataDictionary[country]["table7"][1] = [
        safeFormat(indicator(ctryDat, "stunting_plan")),
        safeFormat(indicator(ctryDat, "anaemia_plan")),
        safeFormat(indicator(ctryDat, "LBW_plan")),
        safeFormat(indicator(ctryDat, "child_overweight_plan")),
        safeFormat(indicator(ctryDat, "EBF_plan")),
        safeFormat(indicator(ctryDat, "wasting_plan")),
        safeFormat(indicator(ctryDat, "sodium_plan")),
        safeFormat(indicator(ctryDat, "overweight_adults_adoles_plan")),
    ]

    dataDictionary[country]["table8"][1][1] = safeFormat(indicator_disagg(ctryDat, "diarrhea_zinc", "all"), percent=True)
    dataDictionary[country]["table8"][1][4] = safeFormat(year(ctryDat, "diarrhea_zinc"))

    dataDictionary[country]["table8"][2][1] = safeFormat(indicator_disagg(ctryDat, "vit_a", "gender", "Both"), percent=True)
    dataDictionary[country]["table8"][2][2] = safeFormat(indicator_disagg(ctryDat, "vit_a", "gender", "Boys"), percent=True)
    dataDictionary[country]["table8"][2][3] = safeFormat(indicator_disagg(ctryDat, "vit_a", "gender", "Girls"), percent=True)
    dataDictionary[country]["table8"][2][4] = safeFormat(year(ctryDat, "vit_a"))

    dataDictionary[country]["table8"][3][1] = safeFormat(indicator_disagg(ctryDat, "iron_supp", "gender", "Both"), percent=True)
    dataDictionary[country]["table8"][3][2] = safeFormat(indicator_disagg(ctryDat, "iron_supp", "gender", "Boys"), percent=True)
    dataDictionary[country]["table8"][3][3] = safeFormat(indicator_disagg(ctryDat, "iron_supp", "gender", "Girls"), percent=True)
    dataDictionary[country]["table8"][3][4] = safeFormat(year(ctryDat, "iron_supp"))

    dataDictionary[country]["table8"][4][1] = safeFormat(indicator_disagg(ctryDat, "iron_and_folic", "all"), percent=True)
    dataDictionary[country]["table8"][4][4] = safeFormat(year(ctryDat, "iron_and_folic"))

    dataDictionary[country]["table8"][5][1] = safeFormat(indicator_disagg(ctryDat, "iodised_salt", "all"), percent=True)
    dataDictionary[country]["table8"][5][4] = safeFormat(year(ctryDat, "iodised_salt"))

generic_style = [
    ('TEXTCOLOR', (0, 0), (-1, -1), dark_grey),
    ('BACKGROUND', (0, 0), (-1, -1), "white"),
    ('LINEABOVE', (0, 0), (-1, 0), 1, dark_grey),
    ('ALIGN', (0, 0), (-1, -1), "LEFT"),
    ('VALIGN', (0, 0), (-1, -1), "MIDDLE"),
    ('LINEBELOW', (0, -1), (-1, -1), 1, dark_grey)
]

tableStyles = {}
tableStyles["table1"] = [
    ('TEXTCOLOR', (0, 0), (-1, -1), dark_grey),
    ('BACKGROUND', (0, 0), (-1, -1), "transparent"),
    ('ALIGN', (0, 0), (-1, -1), "LEFT"),
    ('VALIGN', (0, 0), (-1, -1), "MIDDLE"),
    ('FONTNAME', (0, 0), (-1, 0), "Averta-Bold")
]
tableStyles["table1a"] = tableStyles["table1"]
tableStyles["table2"] = generic_style + [
    ('FONTNAME', (0, 0), (-1, 0), "Averta-Bold"),
    ('LINEABOVE', (0, 1), (-1, 1), 1, dark_grey)
]
tableStyles["table3"] = generic_style + [
    ('LINEABOVE', (0, 1), (-1, 1), 1, grey),
    ('LINEABOVE', (0, 2), (-1, 2), 1, grey),
    ('LINEABOVE', (0, 3), (-1, 3), 1, grey),
    ('FONTNAME', (0, 0), (0, -1), "Averta-Bold")
]
tableStyles["table4"] = generic_style + [
    ('LINEABOVE', (0, 1), (-1, 1), 1, grey),
    ('LINEABOVE', (0, 2), (-1, 2), 1, grey),
    ('LINEABOVE', (0, 3), (-1, 3), 1, grey)
]
tableStyles["table5"] = generic_style + [
    ('LINEABOVE', (0, 1), (-1, 1), 1, grey),
    ('LINEABOVE', (0, 2), (-1, 2), 1, grey)
]
tableStyles["table6"] = generic_style + [
    ('LINEABOVE', (0, 1), (-1, 1), 1, grey),
    ('LINEABOVE', (0, 2), (-1, 2), 1, grey),
    ('LINEABOVE', (0, 3), (-1, 3), 1, grey)
]
tableStyles["table7"] = generic_style + [
    ('TEXTCOLOR', (0, 0), (-1, -1), dark_grey),
    ('BACKGROUND', (0, 0), (-1, -1), "white"),
    ('ALIGN', (0, 0), (-1, -1), "CENTER"),
    ('VALIGN', (0, 0), (-1, -1), "MIDDLE"),
    ('LINEAFTER', (0, 0), (0, -1), 1, dark_grey),
    ('LINEAFTER', (1, 0), (1, -1), 1, dark_grey),
    ('LINEAFTER', (2, 0), (2, -1), 1, dark_grey),
    ('LINEAFTER', (3, 0), (3, -1), 1, dark_grey),
    ('LINEAFTER', (4, 0), (4, -1), 1, dark_grey),
    ('LINEAFTER', (5, 0), (5, -1), 1, dark_grey),
    ('LINEAFTER', (6, 0), (6, -1), 1, dark_grey),
]
tableStyles["table8"] = generic_style + [
    ('FONTNAME', (0, 0), (-1, 0), "Averta-Bold"),
    ('LINEABOVE', (0, 1), (-1, 1), 1, dark_grey),
    ('LINEABOVE', (0, 2), (-1, 2), 1, grey),
    ('LINEABOVE', (0, 3), (-1, 3), 1, grey),
    ('LINEABOVE', (0, 4), (-1, 4), 1, grey),
    ('LINEABOVE', (0, 5), (-1, 5), 1, grey),
]
