#!/usr/bin/python
# -*- coding: utf-8 -*-

from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.platypus import Paragraph
from reportlab.lib.enums import TA_LEFT, TA_CENTER, TA_RIGHT
import copy
import pandas as pd
import numbers
import re
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from reportlab.lib.fonts import addMapping
import pdb

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
dark_greyParaStyleRight = ParagraphStyle('dark_greyParaStyle', parent=style['BodyText'], textColor=dark_grey, alignment=TA_RIGHT)
NACourseStyle = ParagraphStyle('offCourseStyle', parent=style['BodyText'], textColor=grey, alignment=TA_LEFT)
offCourseStyle = ParagraphStyle('offCourseStyle', parent=style['BodyText'], textColor=orange, alignment=TA_LEFT)
progressStyle = ParagraphStyle('progressStyle', parent=style['BodyText'], textColor=aqua, alignment=TA_LEFT)
onCourseStyle = ParagraphStyle('onCourseStyle', parent=style['BodyText'], textColor=dark_aqua, alignment=TA_LEFT)

dataDictionary = {"Asia": {}}
dataDictionary["Asia"]["country"] = "Asia"

dataDictionary["Asia"]["table1"] = [
    [
        Paragraph("<b>Under-5 stunting</b>", style=dark_greyParaBold),
        Paragraph("<b>Under-5 wasting</b>", style=dark_greyParaBold),
        Paragraph("<b>Under-5 overweight</b>", style=dark_greyParaBold),
        Paragraph("<b>WRA anaemia</b>", style=dark_greyParaBold),
        Paragraph("<b>Exclusive breastfeeding</b>", style=dark_greyParaBold)
     ],
    ["Off course, some progress", "On course", "Off course, no progress", "Off course", "On course"]
]

dataDictionary["Asia"]["table1a"] = [
    [
        Paragraph("<b>Adult female obesity</b>", style=dark_greyParaBold),
        Paragraph("<b>Adult male obesity</b>", style=dark_greyParaBold),
        Paragraph("<b>Adult female diabetes</b>", style=dark_greyParaBold),
        Paragraph("<b>Adult male diabetes</b>", style=dark_greyParaBold)
     ],
    ["Off course, some progress", "On course", "Off course, no progress", "Off course"]
]
#
# dataDictionary["Asia"]["table2"] = [
#     [Paragraph("Gini index score<super>1</super>", style=dark_greyParaBold), Paragraph("Gini index rank<super>2</super>", style=dark_greyParaBold), "Year"], [51, 125, 2011]
# ]

dataDictionary["Asia"]["table3"] = [
    ["Population (millions)", format(12428, ",d"), 2015],
    ["Under-5 population (millions)", format(1935, ",d"), 2015],
    ["Rural (%)", format(20, ",d"), 2015],
    ["≥65 years (millions)", format(5, ",d"), 2015],
]

dataDictionary["Asia"]["table4"] = [
    [Paragraph("Early childbearing: births by age 18 (%)", style=dark_greyParaBold), "33", "2011"]
]
dataDictionary["Asia"]["table5"] = [
    ["Physicians", "0.117", "2005"],
    ["Nurses and midwives", "1.306", "2005"],
    ["Community health workers", "0.188", "2005"]
]

dataDictionary["Asia"]["table6"] = [
    [Paragraph("Mandatory legislation for salt iodisation", style=dark_greyParaBold), "Yes"],
    [Paragraph("Sugar-sweetened beverage tax", style=dark_greyParaBold), "Yes"],
    [Paragraph("Food-based dietary guidelines", style=dark_greyParaBold), "Yes"],
    [Paragraph("Multisectoral comprehensive nutrition plan", style=dark_greyParaBold), "Yes"],
]

dataDictionary["Asia"]["table7"] = [
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

# dataDictionary["Asia"]["table8"] = [
#     [
#         Paragraph("<b>Coverage/practice indicator</b>", style=dark_greyParaBold),
#         Paragraph("<b>%</b>", style=dark_greyParaBold),
#         Paragraph("<b>Male</b>", style=dark_greyParaBold),
#         Paragraph("<b>Female</b>", style=dark_greyParaBold),
#         Paragraph("<b>Year</b>", style=dark_greyParaBold)
#      ],
#     [u"Children 0\u201359 months with diarrhoea who received zinc treatment", "8.1", "NA", "NA", "2014"],
#     [u"Children 6\u201359 months who received vitamin A supplements in last 6 months", "71.7", "71.6", "71.9", "2014"],
#     [u"Children 6\u201359 months given iron supplements in past 7 days", "2.7", "2.6", "2.7", "2014"],
#     [Paragraph("Women with a birth in last five years who received iron and folic acid during their most recent pregnancy", style=dark_greyParaStyle), "69.4", "", "NA", "2014"],
#     ["Household consumption of any iodised salt", "99.5", "NA", "NA", "2014"],
# ]

dat = pd.read_csv("data_reg.csv")
country_names = dat.region.unique()
for country_name in country_names:
    dataDictionary[country_name] = copy.deepcopy(dataDictionary["Asia"])


def replaceDash(x):
    x = str(x)
    y = re.sub(r"((?:^|[^{])\d+)-(\d+[^}])", u"\\1\u2013\\2", x)
    return y
missingVals = [" ", ".", "", "No data","na","NA"]


def safeFormat(x, commas=False, precision=0, percent=False, divisor=False):
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
        if divisor:
            try:
                x = float(x) / divisor
                if x < 1:
                    precision += 1
            except ValueError:
                return replaceDash(x)
        if not isinstance(x, numbers.Number):
            try:
                x = float(x)
            except ValueError:
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
    try:
        return ctryDat.loc[(ctryDat["indicator"] == indicator)].iloc[0]["value"]
    except IndexError:
        return "NA"

def indicator_sum(ctryDat, indicator):
    try:
        return ctryDat.loc[(ctryDat["indicator"] == indicator)].iloc[0]["value.sum"]
    except IndexError:
        return "NA"


def indicator_n(ctryDat, indicator):
    try:
        return ctryDat.loc[(ctryDat["indicator"] == indicator)].iloc[0]["n"]
    except IndexError:
        return "NA"


def indicator_n_max(ctryDat, indicator):
    try:
        row = ctryDat.loc[(ctryDat["indicator"] == indicator)]
        max_year = row["year"].max()
        max_row = row[(row["year"] == max_year)].iloc[0]
        return max_row["n"]
    except IndexError:
        return "NA"


def indicator_n_disagg(ctryDat, indicator, disaggregation):
    try:
        return ctryDat.loc[(ctryDat["indicator"] == indicator) & (ctryDat["disaggregation"] == disaggregation)].iloc[0]["n"]
    except IndexError:
        return "NA"


def indicator_frac(ctryDat, indicator, value):
    try:
        row = ctryDat.loc[(ctryDat["indicator"] == indicator) & (ctryDat["value"] == value)].iloc[0]
        return "{}/{}".format(int(row["n"]), int(row["N"]))
    except IndexError:
        return "NA"


def indicator_disagg(ctryDat, indicator, disagg, disagg_value=None):
    if disagg_value:
        try:
            return ctryDat.loc[(ctryDat["indicator"] == indicator) & (ctryDat["disaggregation"] == disagg) & (ctryDat["disagg.value"] == disagg_value)].iloc[0]["value"]
        except IndexError:
            return "NA"
    else:
        try:
            return ctryDat.loc[(ctryDat["indicator"] == indicator) & (ctryDat["disaggregation"] == disagg)].iloc[0]["value"]
        except IndexError:
            return "NA"

def indicator_disagg_frac(ctryDat, indicator, disagg, value, disagg_value=None):
    if disagg_value:
        try:
            row = ctryDat.loc[(ctryDat["indicator"] == indicator) & (ctryDat["disaggregation"] == disagg) & (ctryDat["disagg.value"] == disagg_value) & (ctryDat["value"] == value)].iloc[0]
            return "{}/{}".format(int(row["n"]), int(row["N"]))
        except IndexError:
            return "NA"
    else:
        try:
            row = ctryDat.loc[(ctryDat["indicator"] == indicator) & (ctryDat["disaggregation"] == disagg) & (ctryDat["value"] == value)].iloc[0]
            return "{}/{}".format(int(row["n"]), int(row["N"]))
        except IndexError:
            return "NA"


def year(ctryDat, indicator):
    try:
        return ctryDat.loc[(ctryDat["indicator"] == indicator)].iloc[0]["year"]
    except IndexError:
        return "NA"


def year_range(ctryDat, indicator):
    try:
        return ctryDat.loc[(ctryDat["indicator"] == indicator)].iloc[0]["year_range"]
    except IndexError:
        return "NA"

for country in dataDictionary.keys():
    ctryDat = dat.loc[(dat.region == country)]
    dataDictionary[country]["country"] = country
    dataDictionary[country]["regional"] = ctryDat.iloc[0]["regional"]
    dataDictionary[country]["table1"][1] = [
        Paragraph(safeFormat(indicator_frac(ctryDat, "under_5_stunting_track", "On course"))+" on course", style=offCourseStyle),
        Paragraph(safeFormat(indicator_frac(ctryDat, "under_5_wasting_track", "On course"))+" on course", style=offCourseStyle),
        Paragraph(safeFormat(indicator_frac(ctryDat, "under_5_overweight_track", "On course"))+" on course", style=offCourseStyle),
        Paragraph(safeFormat(indicator_frac(ctryDat, "wra_anaemia_track", "On course"))+" on course", style=offCourseStyle),
        Paragraph(safeFormat(indicator_frac(ctryDat, "ebf_track", "On course"))+" on course", style=offCourseStyle),
    ]

    dataDictionary[country]["table1a"][1] = [
        Paragraph(safeFormat(indicator_frac(ctryDat, "adult_fem_obesity_track", "On course"))+" on course", style=offCourseStyle),
        Paragraph(safeFormat(indicator_frac(ctryDat, "adult_mal_obesity_track", "On course"))+" on course", style=offCourseStyle),
        Paragraph(safeFormat(indicator_frac(ctryDat, "adult_fem_diabetes_track", "On course"))+" on course", style=offCourseStyle),
        Paragraph(safeFormat(indicator_frac(ctryDat, "adult_mal_diabetes_track", "On course"))+" on course", style=offCourseStyle),
    ]

    # dataDictionary[country]["table2"][1] = [
    #     safeFormat(indicator(ctryDat, "gini")),
    #     safeFormat(indicator(ctryDat, "gini_rank")),
    #     safeFormat(year(ctryDat, "gini"))
    # ]

    dataDictionary[country]["table3"][0][1] = safeFormat(indicator_sum(ctryDat, "population"), True, divisor=1000) if safeFormat(indicator_sum(ctryDat, "population"), True, divisor=1000) != "0.0" else "<100,000"
    dataDictionary[country]["table3"][0][2] = safeFormat(year(ctryDat, "population"))
    dataDictionary[country]["table3"][1][1] = safeFormat(indicator_sum(ctryDat, "u5_pop"), True, divisor=1000) if safeFormat(indicator_sum(ctryDat, "u5_pop"), True, divisor=1000) != "0.0" else "<100,000"
    dataDictionary[country]["table3"][1][2] = safeFormat(year(ctryDat, "u5_pop"))
    dataDictionary[country]["table3"][2][1] = safeFormat(indicator(ctryDat, "rural_percent"))
    dataDictionary[country]["table3"][2][2] = safeFormat(year(ctryDat, "rural_percent"))
    dataDictionary[country]["table3"][3][1] = safeFormat(indicator_sum(ctryDat, "65_years"), True, divisor=1000) if safeFormat(indicator_sum(ctryDat, "65_years"), True, divisor=1000) != "0.0" else "<100,000"
    dataDictionary[country]["table3"][3][2] = safeFormat(year(ctryDat, "65_years"))
    dataDictionary[country]["table3_n"] = safeFormat(indicator_n(ctryDat, "population"))

    dataDictionary[country]["table4"][0][1] = safeFormat(indicator(ctryDat, "early_childbearing_prev"))
    dataDictionary[country]["table4"][0][2] = Paragraph(str(safeFormat(year_range(ctryDat, "early_childbearing_prev"))), style=dark_greyParaStyleRight)
    dataDictionary[country]["table4_n"] = safeFormat(indicator_n(ctryDat, "early_childbearing_prev"))

    dataDictionary[country]["table5"][0][1] = safeFormat(indicator(ctryDat, "physicians"), False, 2)
    dataDictionary[country]["table5"][0][2] = Paragraph(str(safeFormat(year_range(ctryDat, "physicians"))), style=dark_greyParaStyleRight)
    dataDictionary[country]["table5"][1][1] = safeFormat(indicator(ctryDat, "nurses_and_midwives"), False, 2)
    dataDictionary[country]["table5"][1][2] = Paragraph(str(safeFormat(year_range(ctryDat, "nurses_and_midwives"))), style=dark_greyParaStyleRight)
    dataDictionary[country]["table5"][2][1] = safeFormat(indicator(ctryDat, "community_health_workers"), False, 2)
    dataDictionary[country]["table5"][2][2] = Paragraph(str(safeFormat(year_range(ctryDat, "community_health_workers"))), style=dark_greyParaStyleRight)
    dataDictionary[country]["table5_n"] = safeFormat(indicator_n(ctryDat, "physicians"))

    dataDictionary[country]["table6"][0][1] = safeFormat(indicator_frac(ctryDat, "salt_leg", "Yes"))
    dataDictionary[country]["table6"][1][1] = safeFormat(indicator_frac(ctryDat, "sugar_tax", "Yes"))
    dataDictionary[country]["table6"][2][1] = safeFormat(indicator_frac(ctryDat, "fbdg", "Yes"))
    dataDictionary[country]["table6"][3][1] = safeFormat(indicator_frac(ctryDat, "multi_sec", "Yes"))

    dataDictionary[country]["table7"][1] = [
        safeFormat(indicator_frac(ctryDat, "stunting_plan", "Yes")),
        safeFormat(indicator_frac(ctryDat, "anaemia_plan", "Yes")),
        safeFormat(indicator_frac(ctryDat, "LBW_plan", "Yes")),
        safeFormat(indicator_frac(ctryDat, "child_overweight_plan", "Yes")),
        safeFormat(indicator_frac(ctryDat, "EBF_plan", "Yes")),
        safeFormat(indicator_frac(ctryDat, "wasting_plan", "Yes")),
        safeFormat(indicator_frac(ctryDat, "sodium_plan", "Yes")),
        safeFormat(indicator_frac(ctryDat, "overweight_adults_adoles_plan", "Yes")),
    ]

    # dataDictionary[country]["table8"][1][1] = safeFormat(indicator_disagg(ctryDat, "diarrhea_zinc", "all"))
    # dataDictionary[country]["table8"][1][4] = safeFormat(year(ctryDat, "diarrhea_zinc"))
    #
    # dataDictionary[country]["table8"][2][1] = safeFormat(indicator_disagg(ctryDat, "vit_a", "gender", "Both"))
    # dataDictionary[country]["table8"][2][2] = safeFormat(indicator_disagg(ctryDat, "vit_a", "gender", "Boys"))
    # dataDictionary[country]["table8"][2][3] = safeFormat(indicator_disagg(ctryDat, "vit_a", "gender", "Girls"))
    # dataDictionary[country]["table8"][2][4] = safeFormat(year(ctryDat, "vit_a"))
    #
    # dataDictionary[country]["table8"][3][1] = safeFormat(indicator_disagg(ctryDat, "iron_supp", "gender", "Both"))
    # dataDictionary[country]["table8"][3][2] = safeFormat(indicator_disagg(ctryDat, "iron_supp", "gender", "Boys"))
    # dataDictionary[country]["table8"][3][3] = safeFormat(indicator_disagg(ctryDat, "iron_supp", "gender", "Girls"))
    # dataDictionary[country]["table8"][3][4] = safeFormat(year(ctryDat, "iron_supp"))
    #
    # dataDictionary[country]["table8"][4][1] = safeFormat(indicator_disagg(ctryDat, "iron_and_folic", "all"))
    # dataDictionary[country]["table8"][4][3] = safeFormat(indicator_disagg(ctryDat, "iron_and_folic", "all"))
    # dataDictionary[country]["table8"][4][4] = safeFormat(year(ctryDat, "iron_and_folic"))
    #
    # dataDictionary[country]["table8"][5][1] = safeFormat(indicator_disagg(ctryDat, "iodised_salt", "all"))
    # dataDictionary[country]["table8"][5][4] = safeFormat(year(ctryDat, "iodised_salt
    dataDictionary[country]["pov_percent_n"] = safeFormat(indicator_n(ctryDat, "190_percent"))
    n_indicators = [
        "190_percent",
        "GDP_capita_PPP",
        "fruit_veg_availability",
        "female_secondary_enroll_net",
        "basic_water",
        "basic_sanitation",
        "agriculture_expenditure",
        "coexistence",
        "adolescent_underweight",
        "adolescent_overweight",
        "adolescent_obesity",
        "adult_diabetes",
        "adult_overweight",
        "adult_obesity",
        "adult_blood_pressure",
        "adult_anemia",
        "adult_sodium",
        "Calcium"
    ]
    for n_indicator in n_indicators:
        dataDictionary[country][n_indicator+"_n"] = safeFormat(indicator_n(ctryDat, n_indicator))
    n_indicators_disagg = [
        ("stunting_percent", "gender"),
        ("overweight_percent", "gender"),
        ("wasting_percent", "income"),
        ("stunting_percent", "income"),
        ("overweight_percent", "income"),
        ("wasting_percent", "location"),
        ("stunting_percent", "location"),
        ("overweight_percent", "location"),
    ]
    for n_indicator, disagg in n_indicators_disagg:
        dataDictionary[country][n_indicator+"_"+disagg+"_n"] = safeFormat(indicator_n_disagg(ctryDat, n_indicator, disagg))

    c_feeding_n_max = 0
    c_feeding_inds = [
        "continued_breastfeeding_2yr",
        "continued_breastfeeding_1yr",
        "minimum_accept_diet",
        "minimum_diet_diversity",
        "minimum_meal",
        "solid_foods",
        "exclusive_breastfeeding",
        "early_initiation"
    ]
    for ind in c_feeding_inds:
        this_n = indicator_n_max(ctryDat, ind)
        try:
            if this_n > c_feeding_n_max:
                c_feeding_n_max = this_n
        except TypeError:
            pass
    dataDictionary[country]["c_feeding_n"] = safeFormat(c_feeding_n_max)


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
# tableStyles["table2"] = generic_style + [
#     ('FONTNAME', (0, 0), (-1, 0), "Averta-Bold"),
#     ('LINEABOVE', (0, 1), (-1, 1), 1, dark_grey)
# ]
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
    ('LINEABOVE', (0, 2), (-1, 2), 1, grey),
    ('FONTNAME', (0, 0), (0, -1), "Averta-Bold")
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
# tableStyles["table8"] = generic_style + [
#     ('FONTNAME', (0, 0), (-1, 0), "Averta-Bold"),
#     ('LINEABOVE', (0, 1), (-1, 1), 1, dark_grey),
#     ('LINEABOVE', (0, 2), (-1, 2), 1, grey),
#     ('LINEABOVE', (0, 3), (-1, 3), 1, grey),
#     ('LINEABOVE', (0, 4), (-1, 4), 1, grey),
#     ('LINEABOVE', (0, 5), (-1, 5), 1, grey),
# ]
