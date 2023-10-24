/* eslint-disable no-undef */
/* eslint-disable no-unused-vars */

/**
 * Search function
 */

const searchInput = document.querySelector("#searchbar > input")
const searchButton = document.querySelector("#searchbar > button")

const lookup = {"/":"/","deepl":"https://deepl.com/","reddit":"https://reddit.com/","maps":"https://maps.google.com/"}
const engine = "https://searx.be/search?q={query}"
const engineUrls = {
  deepl: "https://www.deepl.com/translator#-/-/{query}",
  duckduckgo: "https://duckduckgo.com/?q={query}",
  ecosia: "https://www.ecosia.org/search?q={query}",
  google: "https://www.google.com/search?q={query}",
  startpage: "https://www.startpage.com/search?q={query}",
  youtube: "https://www.youtube.com/results?q={query}",
}

const isWebUrl = value => {
  try {
    const url = new URL(value)
    return url.protocol === "http:" || url.protocol === "https:"
  } catch {
    return false
  }
}

const getTargetUrl = value => {
  if (isWebUrl(value)) return value
  if (lookup[value]) return lookup[value]
  const url = engineUrls[engine] ?? engine
  return url.replace("{query}", value)
}

const search = () => {
  const value = searchInput.value
  const targetUrl = getTargetUrl(value)
  window.open(targetUrl, "_self")
}

searchInput.onkeyup = event => event.key === "Enter" && search()
searchButton.onclick = search

/**
 * inject bookmarks into html
 */

const bookmarks = [{"id":"tWQrvyegshG88b7Z","label":"Social","bookmarks":[{"id":"Q2dlmaeRZDEhD9Qa","label":"Reddit","url":"https://www.reddit.com/"}]},{"id":"xkcDqF4ESKitnrhS","label":"Watch","bookmarks":[{"id":"Fkl9s6mX9Tv0s2Jr","label":"Invidious","url":"https://invidious.perennialte.ch/feed/subscriptions"},{"id":"KiHsu4MUsoqlKOYo","label":"Youtube","url":"https://www.youtube.com/"},{"id":"7qAYuH5ldlqntSXu","label":"Odysee","url":"https://odysee.com/"},{"id":"FYWWUw2jbJRgFHgG","label":"YesMovies","url":"https://yesmovies.mn/"}]},{"id":"Xkf5i41H7vXnDBkH","label":"E-Mail","bookmarks":[{"id":"PsvsgrJ96dNiRE4n","label":"Proton Mail","url":"https://mail.proton.me/u/0/inbox/tC6O0kCJMw1W0SlBM0-4NDVdJtZeCqTucpffIhflc0gg_yO39D1rgCP4eC_2WWAtMIFZ8Q7sYdjdSnFs5rexqw=="},{"id":"FTVFNIF2M98MXjBv","label":"Outlook","url":"https://outlook.live.com/mail/0/"},{"id":"AChQmerv365wTEJD","label":"Mail.com","url":"google.com"}]},{"id":"sFlTDIc4gbqVsDQ4","label":"Server","bookmarks":[{"id":"z1MpYrJxdM9xaNyr","label":"Portainer","url":"https://10.1.1.80:9443/#!/2/docker/containers"},{"id":"Lm5uvYMGI7aSIPBl","label":"Heimdall","url":"https://10.1.1.80:32772/"},{"id":"lXr5hk3r8ToFTZxj","label":"Jellyfin","url":"http://10.1.1.80:8096/"},{"id":"S2EMFzML6MZKyo53","label":"FreshRSS","url":"http://10.1.1.80:32771/i/?rid=64ce32548e0dd"}]},{"id":"CCMX75JUhaYvaKCz","label":"Utils","bookmarks":[{"id":"eHoD9eNOrWk2axMP","label":"Pihole","url":"http://10.1.1.160/admin/"},{"id":"RwZ633hm9ilanWEo","label":"Syncthing Local","url":"http://127.0.0.1:8384/#settings-gui"},{"id":"IXzBXDRcuwzF33SF","label":"Syncthing Server","url":"http://10.1.1.80:8384/"}]}]

const createGroupContainer = () => {
  const container = document.createElement("div")
  container.className = "bookmark-group"
  return container
}

const createGroupTitle = title => {
  const h2 = document.createElement("h2")
  h2.innerHTML = title
  return h2
}

const createBookmark = ({ label, url }) => {
  const li = document.createElement("li")
  const a = document.createElement("a")
  a.href = url
  a.innerHTML = label
  li.append(a)
  return li
}

const createBookmarkList = bookmarks => {
  const ul = document.createElement("ul")
  bookmarks.map(createBookmark).forEach(li => ul.append(li))
  return ul
}

const createGroup = ({ label, bookmarks }) => {
  const container = createGroupContainer()
  const title = createGroupTitle(label)
  const bookmarkList = createBookmarkList(bookmarks)
  container.append(title)
  container.append(bookmarkList)
  return container
}

const injectBookmarks = () => {
  const bookmarksContainer = document.getElementById("bookmarks")
  bookmarksContainer.append()
  bookmarks.map(createGroup).forEach(group => bookmarksContainer.append(group))
}

injectBookmarks()
