import jinja2
import pandas as pd
import progressbar
from PIL import Image as PILImage
from os.path import basename, dirname, isfile
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.pdfgen import canvas
from reportlab.platypus import Image, Paragraph
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from reportlab.lib.fonts import addMapping
from xml.etree import ElementTree


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


########################################################################
class ReportMaker(object):
    """"""

    #----------------------------------------------------------------------
    def __init__(self, countries, template_folder, pdf_file):
        pdfmetrics.registerFont(TTFont('Geomanist', 'fonts/Geomanist-Regular.ttf'))
        pdfmetrics.registerFont(TTFont('Geomanist-Bold', 'fonts/Geomanist-Bold.ttf'))
        pdfmetrics.registerFont(TTFont('Geomanist-Italic', 'fonts/Geomanist-RegularItalic.ttf'))
        addMapping('Geomanist',0,0,'Geomanist')
        addMapping('Geomanist',0,1,'Geomanist-Italic')
        addMapping('Geomanist',1,0,'Geomanist-Bold')

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
        TEMPLATE_FILE = "template.xml.j2"
        template = templateEnv.get_template(TEMPLATE_FILE)

        output = template.render(countries=countries)
        xml_file = "./render/template.xml"
        with open(xml_file, "w") as outfile:
            outfile.write(output)

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
        for page in progressbar.progressbar(self.e.findall("page")):
            self.width, self.height =  int(page.get("width")), int(page.get("height"))
            self.c.setPageSize((self.width,self.height))
            for image in page.findall("image"):
                src = self.template_folder+"/"+image.get("src")
                if "charts" in src:
                    chart_name = basename(src)
                    chart_path = dirname(src)
                    dest = chart_path+"/reduced_"+chart_name
                    if not isfile(dest):
                        pilImg = PILImage.open(src)
                        size = (pilImg.size[0]/1.5,pilImg.size[1]/1.5)
                        pilImg.thumbnail(size,PILImage.NEAREST)
                        pilImg.save(dest,optimize=True)
                else:
                    dest = src
                logo = Image(dest)
                logo.drawHeight = int(image.get("height"))
                logo.drawWidth = int(image.get("width"))
                logo.wrapOn(self.c, self.width, self.height)
                logo.drawOn(self.c, *self.coord(int(image.get("left")),int(image.get("top"))+int(image.get("height")) ))
            for text in page.findall("text"):
                if len(text.getchildren())==0:
                    font = self.fonts[text.get("font")]
                    replacement = text.text

                    if text.get("shrink"):
                        textLen = float(len(replacement))
                        fontSizeAdj = int(font["size"])
                        heightAdj = int(text.get("height"))*2 if textLen > 30 else int(text.get("height"))
                        width = int(text.get("width"))
                    else:
                        fontSizeAdj = int(font["size"])
                        heightAdj = int(text.get("height"))
                        width = self.width

                    style = ParagraphStyle(
                        'default',
                        fontName="Geomanist",
                        leading=fontSizeAdj,
                        fontSize=fontSizeAdj,
                        borderPadding = int(font["padding"]),
                        textColor=font["color"],
                        backColor=font["background"],
                        firstLineIndent=int(font["indent"]),
                    )

                    self.createParagraph(replacement, int(text.get("left")), (int(text.get("top"))+heightAdj), width,style)
                else:
                    innerText = ElementTree.tostring(text.getchildren()[0])
                    font = self.fonts[text.get("font")]
                    replacement = innerText

                    if text.get("shrink"):
                        textLen = float(len(replacement))
                        fontSizeAdj = int(font["size"])
                        heightAdj = int(text.get("height"))*2 if textLen > 30 else int(text.get("height"))
                        width = int(text.get("width"))
                    else:
                        fontSizeAdj = int(font["size"])
                        heightAdj = int(text.get("height"))
                        width = self.width

                    style = ParagraphStyle(
                        'default',
                        fontName="Geomanist",
                        leading=fontSizeAdj,
                        fontSize=fontSizeAdj,
                        borderPadding = int(font["padding"]),
                        textColor=font["color"],
                        backColor=font["background"],
                        firstLineIndent=int(font["indent"]),
                    )

                    self.createParagraph(replacement, int(text.get("left")), (int(text.get("top"))+heightAdj), width, style)
            for line in page.findall("line"):
                self.c.setDash(int(line.get("on")),int(line.get("off")))
                self.c.setStrokeColor(line.get("color"))
                self.c.line(int(line.get("x1")),self.height-int(line.get("y1")),int(line.get("x2")),self.height-int(line.get("y2")))
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
                self.c.bookmarkPage(bookmark.get("name"),fit="FitR",left=1,right=self.width, bottom=self.height-self.width, top=self.height)

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
    def createParagraph(self, ptext, x, y, width, style=None):
        """"""
        if not style:
            style = self.styles["Normal"]
        p = Paragraph(ptext, style=style)
        p.wrapOn(self.c, width, self.height)
        p.drawOn(self.c, *self.coord(x, y))

    #----------------------------------------------------------------------
    def savePDF(self):
        """"""
        self.c.save()

#----------------------------------------------------------------------
if __name__ == "__main__":
    csv_file = "./data/countries_merged.csv"
    countries_df = pd.read_csv(csv_file, keep_default_na=False, na_values=[""])
    countries_df = countries_df.where(countries_df.notnull(), None)
    countries = countries_df.to_dict('records')
    doc = ReportMaker(countries, "./final_template", "./render/p20_profiles.pdf")
    doc.createDocument()
    doc.savePDF()
