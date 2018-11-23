import re
import jinja2
from PIL import Image as PILImage
import os
from os.path import basename, dirname, isfile
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.pdfgen import canvas
from reportlab.platypus import Image, Paragraph, Table
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from reportlab.lib.fonts import addMapping
from xml.etree import ElementTree
from tables_world import dataDictionary, tableStyles


def year(num):
    if num is not None:
        return "{:}".format(round(num))
    return "NA"


def people(num):
    if num is not None:
        return "{:,}".format(round(num))
    return "NA"


def dollar(num):
    if num is not None:
        return "${0:,.2f}".format(float(num))
    return "NA"


def roundDollar(num):
    if num is not None:
        return "${0:,}".format(round(num))
    return "NA"


def percent(num):
    if num is not None:
        return "{}%".format(round(num))
    return "NA"


def roundInt(num):
    if num is not None:
        return round(float(num))
    return "NA"


class ReportMaker(object):
    """"""

    def __init__(self, template_folder, pdf_file, country_name, country_data, template_file="template.xml.j2"):
        pdfmetrics.registerFont(TTFont('Averta', 'fonts/Averta-Regular.ttf'))
        pdfmetrics.registerFont(TTFont('Averta-Bold', 'fonts/Averta-Bold.ttf'))
        pdfmetrics.registerFont(TTFont('Averta-Italic', 'fonts/Averta-RegularItalic.ttf'))
        addMapping('Averta', 0, 0, 'Averta')
        addMapping('Averta', 0, 1, 'Averta-Italic')
        addMapping('Averta', 1, 0, 'Averta-Bold')

        self.country = country_name
        self.dir_path = dirname(os.path.realpath(__file__))
        self.styles = getSampleStyleSheet()
        self.template_folder = template_folder
        templateLoader = jinja2.FileSystemLoader(searchpath="./{}/".format(self.template_folder))
        templateEnv = jinja2.Environment(loader=templateLoader)
        templateEnv.filters['people'] = people
        templateEnv.filters['dollar'] = dollar
        templateEnv.filters['roundDollar'] = roundDollar
        templateEnv.filters['percent'] = percent
        templateEnv.filters['year'] = year
        templateEnv.filters['roundInt'] = roundInt
        template = templateEnv.get_template(template_file)

        xml_output = template.render(country_data=country_data, template_folder=template_folder)

        self.e = ElementTree.ElementTree(ElementTree.fromstring(xml_output)).getroot()
        self.width, self.height = int(self.e.getchildren()[0].get("width")), int(self.e.getchildren()[0].get("height"))
        self.c = canvas.Canvas(pdf_file, pagesize=(self.width, self.height), pageCompression=1)
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

    def createDocument(self):
        """"""
        for page in self.e.findall("page"):
            self.width, self.height = int(page.get("width")), int(page.get("height"))
            self.c.setPageSize((self.width, self.height))
            for image in page.findall("image"):
                src = os.path.join(self.dir_path, image.get("src"))
                # if "charts" in src:
                #     chart_name = basename(src)
                #     chart_path = dirname(src)
                #     dest = chart_path+"/reduced_"+chart_name
                #     if not isfile(dest):
                #         pilImg = PILImage.open(src)
                #         size = (pilImg.size[0]/1.5, pilImg.size[1]/1.5)
                #         pilImg.thumbnail(size, PILImage.NEAREST)
                #         pilImg.save(dest, optimize=True)
                # else:
                dest = src
                logo = Image(dest)
                logo.drawHeight = int(image.get("height"))
                logo.drawWidth = int(image.get("width"))
                logo.wrapOn(self.c, self.width, self.height)
                logo.drawOn(self.c, *self.coord(int(image.get("left")), int(image.get("top"))+int(image.get("height"))))
            for text in page.findall("text"):
                if len(text.getchildren()) == 0:
                    font = self.fonts[text.get("font")]
                    replacement = text.text

                    if text.get("shrink"):
                        fontSize = float(font["size"])
                        height = int(text.get("height"))
                        textLen = float(len(replacement))
                        divisor = max(((textLen/35.0)+(2.0/3.0)), 1)
                        fontSizeAdj = int(fontSize / divisor)
                        fontSizeDiff = int(float(fontSize-fontSizeAdj)/2.0)
                        heightAdj = height-fontSizeDiff
                        width = self.width
                    else:
                        fontSizeAdj = int(font["size"])
                        heightAdj = int(text.get("height"))
                        width = self.width

                    style = ParagraphStyle(
                        'default',
                        fontName="Averta",
                        leading=fontSizeAdj,
                        fontSize=fontSizeAdj,
                        borderPadding=int(font["padding"]),
                        textColor=font["color"],
                        backColor=font["background"],
                        firstLineIndent=int(font["indent"]),
                    )

                    self.createParagraph(replacement, int(text.get("left")), (int(text.get("top"))+heightAdj), width, style)
                else:
                    innerText = "".join([ElementTree.tostring(elem).decode('UTF-8') for elem in text.getchildren()])
                    font = self.fonts[text.get("font")]
                    replacement = innerText

                    if text.get("shrink"):
                        fontSize = float(font["size"])
                        height = int(text.get("height"))
                        textLen = float(len(replacement))
                        divisor = max(((textLen/35.0)+(2.0/3.0)), 1)
                        fontSizeAdj = int(fontSize / divisor)
                        fontSizeDiff = int(float(fontSize-fontSizeAdj)/2.0)
                        heightAdj = height-fontSizeDiff
                        width = self.width
                    else:
                        fontSizeAdj = int(font["size"])
                        heightAdj = int(text.get("height"))
                        width = self.width

                    style = ParagraphStyle(
                        'default',
                        fontName="Averta",
                        leading=fontSizeAdj,
                        fontSize=fontSizeAdj,
                        borderPadding=int(font["padding"]),
                        textColor=font["color"],
                        backColor=font["background"],
                        firstLineIndent=int(font["indent"]),
                    )

                    self.createParagraph(replacement, int(text.get("left")), (int(text.get("top"))+heightAdj), width, style)
            for line in page.findall("line"):
                self.c.setDash(int(line.get("on")), int(line.get("off")))
                self.c.setStrokeColor(line.get("color"))
                lineWidth = int(line.get("width")) if line.get("width") else 1
                self.c.setLineWidth(lineWidth)
                self.c.line(int(line.get("x1")), self.height-int(line.get("y1")), int(line.get("x2")), self.height-int(line.get("y2")))
            for button in page.findall("button"):
                padtop = int(button.get("pt")) if button.get("pt") else 0
                padbottom = int(button.get("pb")) if button.get("pb") else 0
                padleft = int(button.get("pl")) if button.get("pl") else 0
                padright = int(button.get("pr")) if button.get("pr") else 0
                top = (self.height - int(button.get("top")))+padtop
                bottom = ((top-padtop) - int(button.get("height")))-padbottom
                left = int(button.get("left"))-padleft
                right = ((left+padleft) + int(button.get("width")))+padright
                rect = (left, bottom, right, top)
                self.c.linkAbsolute("", button.get("href"), rect, Border='[0 0 0]')
            for bookmark in page.findall("bookmark"):
                self.c.bookmarkPage(bookmark.get("name"), fit="FitR", left=1, right=self.width, bottom=self.height-self.width, top=self.height)
            for table in page.findall("table"):
                self.c.setDash(1, 0)
                tabDat = dataDictionary[self.country][table.get("data")]
                if table.get("widths"):
                    colWidths = [float(width) for width in table.get("widths").split(",")]
                else:
                    colWidths = float(table.get("width"))/len(tabDat[0])
                if table.get("heights"):
                    rowHeights = [float(height) for height in table.get("heights").split(",")]
                else:
                    rowHeights = float(table.get("height"))/len(tabDat)
                t = Table(tabDat, colWidths, rowHeights, style=tableStyles[table.get("data")])
                t.wrapOn(self.c, self.width, self.height)
                t.drawOn(self.c, *self.coord(int(table.get("left")), int(table.get("top"))+int(table.get("height"))))

            self.c.showPage()

    def coord(self, x, y, unit=1):
        """
        # http://stackoverflow.com/questions/4726011/wrap-text-in-a-table-reportlab
        Helper class to help position flowables in Canvas objects
        """
        x, y = x * unit, self.height - y * unit
        return x, y

    def createParagraph(self, ptext, x, y, width, style=None):
        """"""
        if not style:
            style = self.styles["Normal"]
        p = Paragraph(ptext, style=style)
        p.wrapOn(self.c, width, self.height)
        p.drawOn(self.c, *self.coord(x, y))

    def savePDF(self):
        """"""
        self.c.save()

if __name__ == "__main__":
    countries = list(dataDictionary.keys())
    countries.sort()
    for country in countries:
        print(country)
        safeFileName = "".join([c for c in country.replace(" ", "_") if re.match(r'\w', c)])
        countryDat = dataDictionary[country]
        doc = ReportMaker("2018_template_world", "pdfs_world/"+safeFileName+".pdf", country, countryDat)
        doc.createDocument()
        doc.savePDF()
