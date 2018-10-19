#!/usr/bin/python
# -*- coding: utf-8 -*-

from reportlab.lib.pagesizes import letter
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.units import mm, inch
from reportlab.pdfgen import canvas
from reportlab.platypus import Image, Paragraph, Table
from xml.etree import ElementTree
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from reportlab.lib.fonts import addMapping
from tables import dataDictionary, tableStyles
import pdb
import re
from PIL import Image as PILImage
# import sys
# reload(sys)
# sys.setdefaultencoding("utf-8")


########################################################################
class ReportMaker(object):
    """"""

    #----------------------------------------------------------------------
    def __init__(self, pdf_file, xml_file,country_name):
        pdfmetrics.registerFont(TTFont('Arial', 'fonts/Arial.ttf'))
        pdfmetrics.registerFont(TTFont('Arial-Bold', 'fonts/Arial-Bold.ttf'))
        pdfmetrics.registerFont(TTFont('Arial-Italic', 'fonts/Arial-Italic.ttf'))
        addMapping('Arial',0,0,'Arial')
        addMapping('Arial',0,1,'Arial-Italic')
        addMapping('Arial',1,0,'Arial-Bold')

        self.country = country_name
        self.styles = getSampleStyleSheet()
        self.e = ElementTree.parse(xml_file).getroot()
        self.width, self.height =  int(self.e.getchildren()[0].get("width")), int(self.e.getchildren()[0].get("height"))
        self.c = canvas.Canvas(pdf_file, pagesize=(self.width,self.height),pageCompression=1)
        self.fonts = {}
        for page in self.e.findall("page"):
            for fontspec in page.findall("fontspec"):
                font = {}
                font["size"] = int(fontspec.get("size"))
                font["color"] = fontspec.get("color")
                font["background"] = fontspec.get("background")
                if fontspec.get("indent") is not None:
                    font["indent"] = fontspec.get("indent")
                else:
                    font["indent"] = "0"
                if fontspec.get("padding") is not None:
                    font["padding"] = fontspec.get("padding")
                else:
                    font["padding"] = "0"
                self.fonts[fontspec.get("id")] = font

    #----------------------------------------------------------------------
    def createDocument(self):
        """"""
        for page in self.e.findall("page"):
            for image in page.findall("image"):
                if image.get("variable")=="True":
                    src = image.get("path")+self.country+"/"+image.get("src")
                    dest = image.get("path")+self.country+"/reduced_"+image.get("src")
                    pilImg = PILImage.open(src)
                    size = (pilImg.size[0]/1.5,pilImg.size[1]/1.5)
                    pilImg.thumbnail(size,PILImage.NEAREST)
                    pilImg.save(dest,optimize=True)
                else:
                    src = image.get("path")+image.get("src")
                    dest = src
                # logo = Image(dest)
                logo = Image(src)
                logo.drawHeight = int(image.get("height"))
                logo.drawWidth = int(image.get("width"))
                logo.wrapOn(self.c, self.width, self.height)
                logo.drawOn(self.c, *self.coord(int(image.get("left")),int(image.get("top"))+int(image.get("height")) ))
            for text in page.findall("text"):
                if len(text.getchildren())==0:
                    font = self.fonts[text.get("font")]
                    if text.get("replace"):
                        replacement = dataDictionary[self.country][text.get("replace")]
                    else:
                        replacement = text.text

                    if text.get("shrink"):
                        fontSize = float(font["size"])
                        height = int(text.get("height"))
                        textLen = float(len(replacement))
                        divisor = max(((textLen/25.0)+(2.0/3.0)),1)
                        fontSizeAdj = int(fontSize / divisor)
                        fontSizeDiff = int(float(fontSize-fontSizeAdj)/2.0)
                        heightAdj = height-fontSizeDiff
                    else:
                        fontSizeAdj = int(font["size"])
                        heightAdj = int(text.get("height"))

                    style = ParagraphStyle(
                        'default',
                        fontName="Arial",
                        leading=fontSizeAdj,
                        fontSize=fontSizeAdj,
                        borderPadding = int(font["padding"]),
                        textColor=font["color"],
                        backColor=font["background"],
                        firstLineIndent=int(font["indent"]),
                    )

                    self.createParagraph(replacement, int(text.get("left")), (int(text.get("top"))+heightAdj),style)
                else:
                    innerText = ElementTree.tostring(text.getchildren()[0])
                    font = self.fonts[text.get("font")]
                    if text.get("replace"):
                        replacement = dataDictionary[self.country][text.get("replace")]
                    else:
                        replacement = innerText

                    if text.get("shrink"):
                        fontSize = float(font["size"])
                        height = int(text.get("height"))
                        textLen = float(len(replacement))
                        divisor = max(((textLen/25.0)+(2.0/3.0)),1)
                        fontSizeAdj = int(fontSize / divisor)
                        fontSizeDiff = int(float(fontSize-fontSizeAdj)/2.0)
                        heightAdj = height-fontSizeDiff
                    else:
                        fontSizeAdj = int(font["size"])
                        heightAdj = int(text.get("height"))

                    style = ParagraphStyle(
                        'default',
                        fontName="Arial",
                        leading=fontSizeAdj,
                        fontSize=fontSizeAdj,
                        borderPadding = int(font["padding"]),
                        textColor=font["color"],
                        backColor=font["background"],
                        firstLineIndent=int(font["indent"]),
                    )

                    self.createParagraph(replacement, int(text.get("left")), (int(text.get("top"))+heightAdj),style)
            for line in page.findall("line"):
                self.c.setDash(int(line.get("on")),int(line.get("off")))
                self.c.setStrokeColor(line.get("color"))
                self.c.line(int(line.get("x1")),self.height-int(line.get("y1")),int(line.get("x2")),self.height-int(line.get("y2")))
            for table in page.findall("table"):
                self.c.setDash(1,0)
                tabDat = dataDictionary[self.country][table.get("data")]
                if table.get("widths"):
                    colWidths = [float(width) for width in table.get("widths").split(",")]
                else:
                    colWidths = float(table.get("width"))/len(tabDat[0])
                if table.get("heights"):
                    rowHeights = [float(height) for height in table.get("heights").split(",")]
                else:
                    rowHeights = float(table.get("height"))/len(tabDat)
                t = Table(tabDat,colWidths,rowHeights,style=tableStyles[table.get("data")])
                t.wrapOn(self.c, self.width, self.height)
                t.drawOn(self.c, *self.coord(int(table.get("left")), int(table.get("top"))+int(table.get("height"))))

            self.c.showPage()

    #----------------------------------------------------------------------
    def coord(self, x, y, unit=1):
        """
        # http://stackoverflow.com/questions/4726011/wrap-text-in-a-table-reportlab
        Helper class to help position flowables in Canvas objects
        """
        x, y = x * unit, self.height -  y * unit
        return x, y

    #----------------------------------------------------------------------
    def createParagraph(self, ptext, x, y, style=None):
        """"""
        if not style:
            style = self.styles["Normal"]
        p = Paragraph(ptext, style=style)
        p.wrapOn(self.c, self.width, self.height)
        p.drawOn(self.c, *self.coord(x, y))

    #----------------------------------------------------------------------
    def savePDF(self):
        """"""
        self.c.save()

#----------------------------------------------------------------------
if __name__ == "__main__":
    countries = dataDictionary.keys()
    countries.sort()
    for country in countries:
        print(country)
        safeFileName = "".join([c for c in country.replace(" ", "_") if re.match(r'\w', c)])
        doc = ReportMaker("C:\\Users\\Alex\\Documents\\Data\\GNR\\Country profile PDFs\\"+safeFileName+".pdf","template.xml",country)
        doc.createDocument()
        doc.savePDF()
