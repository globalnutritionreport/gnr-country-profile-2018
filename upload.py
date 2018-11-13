from selenium import webdriver
import time
import json
import pdb
from selenium.webdriver.remote.command import Command
from optparse import OptionParser
import os

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

browser = webdriver.Chrome("/home/alex/chromedriver")
browser.maximize_window()
browser.implicitly_wait(30) # Configure the WebDriver to wait up to 30 seconds for each page to load

browser.get("https://gnr.fffdev.co/admin/") # Load page
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

browser.find_element_by_xpath('//*[@type="submit"]').click() # Click the submit button

browser.find_element_by_xpath("//*[text()='Profiles']").click()

# Actually need to do Regions and Subregions first, but just testing for now

browser.find_element_by_xpath("//*[text()='Countries']").click()

browser.find_element_by_xpath('//*[@title="Add a new Nutrition profile country"]').click()

radio = browser.find_element_by_xpath("//*[@id='id_parent_page_0']")
scroll_and_click(browser, radio)

browser.find_element_by_xpath("//*[@value='Continue']").click() # Click the submit button

queries = [
    {"input_id": "id_title", "input_str": "Kenya"},
    {"input_id": "id_description", "input_str": "Kenya description"},
    {"input_id": "id_profile_title", "input_str": "Kenya profile title"},
    {"input_id": "id_profile_description", "input_str": "Kenya profile description"},
]

input_text(browser, queries)
pdb.set_trace()
browser.find_element_by_xpath("//*[text()='Choose an image']").click()
