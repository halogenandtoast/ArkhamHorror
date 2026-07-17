const CARD_DATA_FILENAME = /^cards_[a-z]+(?:-[a-z]+)?\.json$/

function isCardDataFilename(filename) {
  return CARD_DATA_FILENAME.test(filename)
}

module.exports = { isCardDataFilename }
