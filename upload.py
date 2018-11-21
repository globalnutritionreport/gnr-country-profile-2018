from selenium import webdriver
import time
import json
from selenium.webdriver.support.ui import Select
from optparse import OptionParser
import pandas as pd
import re
from django.template.defaultfilters import slugify


class Error(Exception):
    """Base class for exceptions in this module."""
    pass


class InputError(Error):
    """Exception raised for errors in the input.

    Attributes:
        msg  -- explanation of the error
    """

    def __init__(self, msg):
        self.msg = msg


def set_element_value(driver, element, value):
    """Set the value of an element.
    Args:
        admin_browser (browser): The splinter browser instance.
        element (ElementAPI): The splinter element to be waited on.
    """
    wait_for_clickability(element)
    driver.execute_script("arguments[0].setAttribute('value', arguments[1]);", element, value)

def wait_for_clickability(element, wait_time=1):
    """Wait until an element is enabled before clicking.
    Args:
        element (ElementAPI): The splinter element to be waited on.
        wait_time (int): The time in seconds to wait.
    """
    end_time = time.time() + wait_time

    while time.time() < end_time:
        if element and element.is_enabled():
            return True
    return False


def wait_for_visibility(element, wait_time=1):
    """Wait until an element is visible before scrolling.
    Args:
        element (ElementAPI): The splinter element to be waited on.
        wait_time (int): The time in seconds to wait.
    """
    end_time = time.time() + wait_time

    while time.time() < end_time:
        if element and element.is_displayed():
            return True
    return False


def click_obscured(driver, element):
    """Click elements even if they're slightly obscured.
    Args:
        admin_browser (browser): The splinter browser instance.
        element (ElementAPI): The splinter element to be waited on.
    """
    wait_for_clickability(element)
    driver.execute_script("arguments[0].click();", element)


def scroll_to_element(driver, element):
    """Scroll to the location of an element.
    Args:
        admin_browser (browser): The splinter browser instance.
        element (ElementAPI): The splinter element to be waited on.
    """
    wait_for_visibility(element)
    rect = driver.execute_script("return arguments[0].getBoundingClientRect();", element)
    mid_point_x = int(rect['x'] + (rect['width'] / 2))
    end_point_y = int(rect['y'] + (rect['height']))
    driver.execute_script("window.scrollTo({}, {});".format(mid_point_x, end_point_y))


def scroll_and_click(driver, element):
    """Scroll to and click an element.
    Args:
        admin_browser (browser): The splinter browser instance.
        element (ElementAPI): The splinter element to be waited on.
    """
    scroll_to_element(driver, element)
    click_obscured(driver, element)


parser = OptionParser()
parser.add_option("-u", "--username", dest="user", default=False,
                help="CMS username", metavar="STRING")
parser.add_option("-p", "--password", dest="password", default=False,
                        help="CMS password",metavar="STRING")
(options, args) = parser.parse_args()


def input_text(browser, inputs):
    # Fills a list of text boxes

    browser.find_element_by_xpath('//*[@id="{}"]'.format(inputs[0]["input_id"]))
    inputs = json.dumps(inputs)
    js = "var inputs = {};".format(inputs)
    js += """
    console.log(inputs)
    for (var k = 0; k < inputs.length; k++) {
        var inputStr = inputs[k]["input_str"];
        var input = document.getElementById(inputs[k]["input_id"]);
        input.value = inputStr;
    }
    return true;"""
    browser.execute_script(js)

if not options.user:
    raise InputError("A valid username was not supplied.")

if not options.password:
    raise InputError("A valid password was not supplied.")

all_dat = pd.read_csv("data.csv")
countries = all_dat.filter(['country', 'region', 'subregion']).drop_duplicates()

nested_countries = {}
for index, row in countries.iterrows():
    region = row['region']
    subregion = row['subregion']
    country = row['country']
    if region in nested_countries.keys():
        if subregion not in nested_countries[region].keys():
            nested_countries[region][subregion] = list()
    else:
        nested_countries[region] = dict()
        nested_countries[region][subregion] = list()
    nested_countries[region][subregion].append(country)

browser = webdriver.Chrome("/home/alex/chromedriver")
browser.maximize_window()
browser.implicitly_wait(30) # Configure the WebDriver to wait up to 30 seconds for each page to load

browser.get("https://gnr.fffdev.co/admin") # Load page
queries = []
userInput = {}
userInput["input_id"] = "id_username"
userInput["input_str"] = options.user
queries.append(userInput)
passInput = {}
passInput["input_id"] = "id_password"
passInput["input_str"] = options.password
queries.append(passInput)
input_text(browser, queries)

submit_button = browser.find_element_by_xpath('//*[@type="submit"]')
scroll_and_click(browser, submit_button)

region_count = -1
subregion_count = -1

img_path = "/home/alex/git/gnr-country-profile-2018/hero.jpg"
description = """Explore the Global Nutrition Report’s nutrition profiles, which capture the status of nutrition at the country, regional and global level.

Photo credit: Curt Carnemark / World Bank"""
profile_description = "Country profiles aggregate the very latest data on child, adolescent and adult anthropometry and nutritional status, as well as intervention coverage, food supply, nutrition spending and demography."


for region_name in nested_countries:
    safeFileName = "".join([c for c in region_name.replace(" ", "_") if re.match(r'\w', c)])
    img_path2 = "/home/alex/git/gnr-country-profile-2018/thumbs_reg/{}-0.jpg".format(safeFileName)
    pdf_path = "/home/alex/git/gnr-country-profile-2018/pdfs_reg/{}.pdf".format(safeFileName)
    title = region_name
    profile_title = "{} {} nutrition profile".format(region_name, "regional")
    seo_title = profile_title

    browser.get("https://gnr.fffdev.co/admin") # Load page

    profile_button = browser.find_element_by_xpath("//*[text()='Profiles']")
    scroll_and_click(browser, profile_button)

    region_button = browser.find_element_by_xpath("//*[text()='Regions']")
    scroll_and_click(browser, region_button)

    add_region_button = browser.find_element_by_xpath('//*[@title="Add a new Nutrition profile region"]')
    scroll_and_click(browser, add_region_button)

    # All text fields
    queries = [
        {"input_id": "id_title", "input_str": title},
        {"input_id": "id_description", "input_str": description},
        {"input_id": "id_profile_title", "input_str": profile_title},
        {"input_id": "id_profile_description", "input_str": profile_description},
        {"input_id": "id_seo_title", "input_str": seo_title},
    ]
    input_text(browser, queries)

    queries = [
        {"input_id": "id_slug", "input_str": slugify(title)},
    ]
    input_text(browser, queries)

    # First image field
    add_image = browser.find_elements_by_xpath("//*[text()='Choose an image']")[0]
    scroll_and_click(browser, add_image)

    upload_button = browser.find_element_by_xpath("//*[text()='Upload']")
    scroll_and_click(browser, upload_button)

    image_title_field = browser.find_elements_by_xpath("//*[@id='id_title']")[1]
    set_element_value(browser, image_title_field, region_name+" hero")

    image_file_field = browser.find_elements_by_xpath("//*[@id='id_file']")[0]
    image_file_field.send_keys(img_path)

    image_collection_field = Select(browser.find_elements_by_xpath("//*[@id='id_collection']")[0])
    image_collection_field.select_by_value('8')

    upload_button = browser.find_elements_by_xpath('//*[@type="submit"]')[4]
    scroll_and_click(browser, upload_button)

    # Document field
    add_document = browser.find_elements_by_xpath("//*[text()='Choose a document']")[0]
    scroll_and_click(browser, add_document)

    upload_button = browser.find_element_by_xpath("//*[text()='Upload']")
    scroll_and_click(browser, upload_button)

    document_title_field = browser.find_elements_by_xpath("//*[@id='id_title']")[1]
    set_element_value(browser, document_title_field, region_name+" regional profile")

    document_file_field = browser.find_elements_by_xpath("//*[@id='id_file']")[0]
    document_file_field.send_keys(pdf_path)

    document_collection_field = Select(browser.find_elements_by_xpath("//*[@id='id_collection']")[0])
    document_collection_field.select_by_value('8')

    upload_button = browser.find_elements_by_xpath('//*[@type="submit"]')[4]
    scroll_and_click(browser, upload_button)

    # second image field
    add_image = browser.find_elements_by_xpath("//*[text()='Choose an image']")[1]
    scroll_and_click(browser, add_image)

    upload_button = browser.find_elements_by_xpath("//*[text()='Upload']")[0]
    scroll_and_click(browser, upload_button)

    image_title_field = browser.find_elements_by_xpath("//*[@id='id_title']")[1]
    set_element_value(browser, image_title_field, region_name+" thumbnail")

    image_file_field = browser.find_elements_by_xpath("//*[@id='id_file']")[0]
    image_file_field.send_keys(img_path2)

    image_collection_field = Select(browser.find_elements_by_xpath("//*[@id='id_collection']")[0])
    image_collection_field.select_by_value('8')

    upload_button = browser.find_elements_by_xpath('//*[@type="submit"]')[4]
    scroll_and_click(browser, upload_button)
    time.sleep(1)

    # publish
    up_arrow = browser.find_elements_by_xpath("//*[@class='dropdown-toggle icon icon-arrow-up']")[0]
    scroll_and_click(browser, up_arrow)

    pub_button = browser.find_element_by_xpath("//*[@name='action-publish']")
    scroll_and_click(browser, pub_button)

    region_count += 1
    for subregion_name in nested_countries[region_name]:
        safeFileName = "".join([c for c in subregion_name.replace(" ", "_") if re.match(r'\w', c)])
        img_path2 = "/home/alex/git/gnr-country-profile-2018/thumbs_reg/{}-0.jpg".format(safeFileName)
        pdf_path = "/home/alex/git/gnr-country-profile-2018/pdfs_reg/{}.pdf".format(safeFileName)
        title = subregion_name
        profile_title = "{} {} nutrition profile".format(subregion_name, "subregional")
        seo_title = profile_title

        browser.get("https://gnr.fffdev.co/admin") # Load page

        profile_button = browser.find_element_by_xpath("//*[text()='Profiles']")
        scroll_and_click(browser, profile_button)

        region_button = browser.find_element_by_xpath("//*[text()='Sub-regions']")
        scroll_and_click(browser, region_button)

        add_region_button = browser.find_element_by_xpath('//*[@title="Add a new Nutrition profile sub region"]')
        scroll_and_click(browser, add_region_button)

        if region_count>=1:
            radio = browser.find_element_by_xpath("//*[@id='id_parent_page_{}']".format(region_count))
            scroll_and_click(browser, radio)

            continue_button = browser.find_element_by_xpath("//*[@value='Continue']")
            scroll_and_click(browser, continue_button)

        # All text fields
        queries = [
            {"input_id": "id_title", "input_str": title},
            {"input_id": "id_description", "input_str": description},
            {"input_id": "id_profile_title", "input_str": profile_title},
            {"input_id": "id_profile_description", "input_str": profile_description},
            {"input_id": "id_seo_title", "input_str": seo_title},
        ]
        input_text(browser, queries)

        queries = [
            {"input_id": "id_slug", "input_str": slugify(title)},
        ]
        input_text(browser, queries)

        # First image field
        add_image = browser.find_elements_by_xpath("//*[text()='Choose an image']")[0]
        scroll_and_click(browser, add_image)

        upload_button = browser.find_element_by_xpath("//*[text()='Upload']")
        scroll_and_click(browser, upload_button)

        image_title_field = browser.find_elements_by_xpath("//*[@id='id_title']")[1]
        set_element_value(browser, image_title_field, subregion_name+" hero")

        image_file_field = browser.find_elements_by_xpath("//*[@id='id_file']")[0]
        image_file_field.send_keys(img_path)

        image_collection_field = Select(browser.find_elements_by_xpath("//*[@id='id_collection']")[0])
        image_collection_field.select_by_value('8')

        upload_button = browser.find_elements_by_xpath('//*[@type="submit"]')[4]
        scroll_and_click(browser, upload_button)

        # Document field
        add_document = browser.find_elements_by_xpath("//*[text()='Choose a document']")[0]
        scroll_and_click(browser, add_document)

        upload_button = browser.find_element_by_xpath("//*[text()='Upload']")
        scroll_and_click(browser, upload_button)

        document_title_field = browser.find_elements_by_xpath("//*[@id='id_title']")[1]
        set_element_value(browser, document_title_field, subregion_name+" subregional profile")

        document_file_field = browser.find_elements_by_xpath("//*[@id='id_file']")[0]
        document_file_field.send_keys(pdf_path)

        document_collection_field = Select(browser.find_elements_by_xpath("//*[@id='id_collection']")[0])
        document_collection_field.select_by_value('8')

        upload_button = browser.find_elements_by_xpath('//*[@type="submit"]')[4]
        scroll_and_click(browser, upload_button)

        # second image field
        add_image = browser.find_elements_by_xpath("//*[text()='Choose an image']")[1]
        scroll_and_click(browser, add_image)

        upload_button = browser.find_elements_by_xpath("//*[text()='Upload']")[0]
        scroll_and_click(browser, upload_button)

        image_title_field = browser.find_elements_by_xpath("//*[@id='id_title']")[1]
        set_element_value(browser, image_title_field, subregion_name+" thumbnail")

        image_file_field = browser.find_elements_by_xpath("//*[@id='id_file']")[0]
        image_file_field.send_keys(img_path2)

        image_collection_field = Select(browser.find_elements_by_xpath("//*[@id='id_collection']")[0])
        image_collection_field.select_by_value('8')

        upload_button = browser.find_elements_by_xpath('//*[@type="submit"]')[4]
        scroll_and_click(browser, upload_button)
        time.sleep(1)

        # publish
        up_arrow = browser.find_elements_by_xpath("//*[@class='dropdown-toggle icon icon-arrow-up']")[0]
        scroll_and_click(browser, up_arrow)

        pub_button = browser.find_element_by_xpath("//*[@name='action-publish']")
        scroll_and_click(browser, pub_button)

        subregion_count += 1
        for country_name in nested_countries[region_name][subregion_name]:
            safeFileName = "".join([c for c in country_name.replace(" ", "_") if re.match(r'\w', c)])
            img_path2 = "/home/alex/git/gnr-country-profile-2018/thumbs/{}-0.jpg".format(safeFileName)
            pdf_path = "/home/alex/git/gnr-country-profile-2018/pdfs/{}.pdf".format(safeFileName)
            title = country_name
            profile_title = "{} {} nutrition profile".format(country_name, "country")
            seo_title = profile_title

            browser.get("https://gnr.fffdev.co/admin") # Load page

            profile_button = browser.find_element_by_xpath("//*[text()='Profiles']")
            scroll_and_click(browser, profile_button)

            region_button = browser.find_element_by_xpath("//*[text()='Countries']")
            scroll_and_click(browser, region_button)

            add_region_button = browser.find_element_by_xpath('//*[@title="Add a new Nutrition profile country"]')
            scroll_and_click(browser, add_region_button)

            if subregion_count>=1:
                radio = browser.find_element_by_xpath("//*[@id='id_parent_page_{}']".format(subregion_count))
                scroll_and_click(browser, radio)

                continue_button = browser.find_element_by_xpath("//*[@value='Continue']")
                scroll_and_click(browser, continue_button)

            # All text fields
            queries = [
                {"input_id": "id_title", "input_str": title},
                {"input_id": "id_description", "input_str": description},
                {"input_id": "id_profile_title", "input_str": profile_title},
                {"input_id": "id_profile_description", "input_str": profile_description},
                {"input_id": "id_seo_title", "input_str": seo_title},
            ]
            input_text(browser, queries)

            queries = [
                {"input_id": "id_slug", "input_str": slugify(title)},
            ]
            input_text(browser, queries)

            # First image field
            add_image = browser.find_elements_by_xpath("//*[text()='Choose an image']")[0]
            scroll_and_click(browser, add_image)

            upload_button = browser.find_element_by_xpath("//*[text()='Upload']")
            scroll_and_click(browser, upload_button)

            image_title_field = browser.find_elements_by_xpath("//*[@id='id_title']")[1]
            set_element_value(browser, image_title_field, country_name+" hero")

            image_file_field = browser.find_elements_by_xpath("//*[@id='id_file']")[0]
            image_file_field.send_keys(img_path)

            image_collection_field = Select(browser.find_elements_by_xpath("//*[@id='id_collection']")[0])
            image_collection_field.select_by_value('8')

            upload_button = browser.find_elements_by_xpath('//*[@type="submit"]')[4]
            scroll_and_click(browser, upload_button)

            # Document field
            add_document = browser.find_elements_by_xpath("//*[text()='Choose a document']")[0]
            scroll_and_click(browser, add_document)

            upload_button = browser.find_element_by_xpath("//*[text()='Upload']")
            scroll_and_click(browser, upload_button)

            document_title_field = browser.find_elements_by_xpath("//*[@id='id_title']")[1]
            set_element_value(browser, document_title_field, country_name+" country profile")

            document_file_field = browser.find_elements_by_xpath("//*[@id='id_file']")[0]
            document_file_field.send_keys(pdf_path)

            document_collection_field = Select(browser.find_elements_by_xpath("//*[@id='id_collection']")[0])
            document_collection_field.select_by_value('8')

            upload_button = browser.find_elements_by_xpath('//*[@type="submit"]')[4]
            scroll_and_click(browser, upload_button)

            # second image field
            add_image = browser.find_elements_by_xpath("//*[text()='Choose an image']")[1]
            scroll_and_click(browser, add_image)

            upload_button = browser.find_elements_by_xpath("//*[text()='Upload']")[0]
            scroll_and_click(browser, upload_button)

            image_title_field = browser.find_elements_by_xpath("//*[@id='id_title']")[1]
            set_element_value(browser, image_title_field, country_name+" thumbnail")

            image_file_field = browser.find_elements_by_xpath("//*[@id='id_file']")[0]
            image_file_field.send_keys(img_path2)

            image_collection_field = Select(browser.find_elements_by_xpath("//*[@id='id_collection']")[0])
            image_collection_field.select_by_value('8')

            upload_button = browser.find_elements_by_xpath('//*[@type="submit"]')[4]
            scroll_and_click(browser, upload_button)
            time.sleep(1)

            # publish
            up_arrow = browser.find_elements_by_xpath("//*[@class='dropdown-toggle icon icon-arrow-up']")[0]
            scroll_and_click(browser, up_arrow)

            pub_button = browser.find_element_by_xpath("//*[@name='action-publish']")
            scroll_and_click(browser, pub_button)

browser.close()
