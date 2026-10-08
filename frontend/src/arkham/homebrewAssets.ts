// Frontend assets contributed by homebrew campaigns, discovered from the
// frontend/homebrew/<campaign>/ directories:
//
// - `*.css` is injected into the bundle as-is (asset URLs inside should be
//   absolute, e.g. /img/arkham/homebrew/<campaign>/...).
// - `icons.json` maps icon keys to CSS classes; each entry hooks the text
//   formatters so `{key}` (i18n/flavor text) and `[key]` (ArkhamDB-style card
//   text) both render as `<span class="<class>"></span>`.
// - `scenario-decks.json` declares campaign-specific deck image behavior and
//   an optional CSS class for display rules owned by that campaign.
// - `fonts.json` maps a CSS font-family name to a font file in the campaign
//   directory; each entry gets an `@font-face` and a `.font-<slug>` class, so
//   flavor text can say `<div class='font-vivaldi'>...</div>`.
//
// Like the locale and instance discovery, dropping a campaign directory in
// requires no registration here.
import.meta.glob('@homebrew/*/*.css', { eager: true })

export type HomebrewScenarioDeckDisplay = {
  image?: 'top-card-back'
  className?: string
}

const scenarioDeckModules = import.meta.glob('@homebrew/*/scenario-decks.json', { eager: true }) as Record<
  string,
  { default: Record<string, HomebrewScenarioDeckDisplay> }
>

const homebrewScenarioDeckDisplays: Record<string, HomebrewScenarioDeckDisplay> = Object.assign(
  {},
  ...Object.values(scenarioDeckModules).map((m) => m.default),
)

export function homebrewScenarioDeckDisplay(deckKey: string): HomebrewScenarioDeckDisplay | undefined {
  return homebrewScenarioDeckDisplays[deckKey]
}

const iconModules = import.meta.glob('@homebrew/*/icons.json', { eager: true }) as Record<
  string,
  { default: Record<string, string> }
>

export const homebrewIcons: Record<string, string> = Object.assign(
  {},
  ...Object.values(iconModules).map((m) => m.default),
)

// { '[moon]': '<span class="moon-icon"></span>', ... } for TOKEN_MAP-style maps
export const homebrewTokenMap: Record<string, string> = Object.fromEntries(
  Object.entries(homebrewIcons).map(([key, cls]) => [`[${key}]`, `<span class="${cls}"></span>`]),
)

// Applies {key} replacements for every homebrew icon
export function replaceHomebrewIcons(body: string): string {
  return Object.entries(homebrewIcons).reduce(
    (acc, [key, cls]) => acc.replaceAll(`{${key}}`, `<span class="${cls}"></span>`),
    body,
  )
}

// Font files anywhere under a campaign directory, as emitted asset URLs. Only
// the URL string lands in the bundle -- the browser fetches the file when a
// rule that uses the family actually applies.
const fontFileModules = import.meta.glob('@homebrew/**/*.{ttf,otf,woff,woff2}', {
  eager: true,
  query: '?url',
  import: 'default',
}) as Record<string, string>

// A font is declared as its file, or as an object when the face needs settings
// of its own: `size`, because a display script set at body size reads as a
// mistake, and `stroke`, a hairline that thickens a face shipping only one
// weight. `font-weight` is no use for those -- the browser's synthetic bold is
// all-or-nothing and smears a script -- so a family that really does have a
// bold should register that file as its own entry instead.
export type HomebrewFontDecl = string | { file: string; size?: string; stroke?: string }

const fontManifestModules = import.meta.glob('@homebrew/*/fonts.json', { eager: true }) as Record<
  string,
  { default: Record<string, HomebrewFontDecl> }
>

export type HomebrewFont = {
  family: string
  className: string
  url: string
  size?: string
  stroke?: string
}

const fontSlug = (family: string) =>
  family.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-+|-+$/g, '')

export const homebrewFonts: HomebrewFont[] = Object.entries(fontManifestModules).flatMap(
  ([manifestPath, manifest]) => {
    const campaign = manifestPath.split('/').at(-2)
    return Object.entries(manifest.default).flatMap(([family, decl]) => {
      const { file, size, stroke } = typeof decl === 'string' ? { file: decl } : decl
      // Glob keys are resolved paths, so match on the campaign-relative tail
      // rather than assuming how the `@homebrew` alias was expanded.
      const suffix = `/${campaign}/${file}`
      const found = Object.entries(fontFileModules).find(([p]) => p.endsWith(suffix))
      if (!found) {
        console.warn(`[homebrew] ${campaign}: fonts.json points at a missing file: ${file}`)
        return []
      }
      return [{ family, className: `font-${fontSlug(family)}`, url: found[1], size, stroke }]
    })
  },
)

export function homebrewFontCss(fonts: HomebrewFont[] = homebrewFonts): string {
  return fonts
    .map(({ family, className, url, size, stroke }) => {
      // A scaled face needs `line-height: 1` too: set inline at 2em inside an
      // ordinary paragraph it would otherwise overlap the line above it.
      const scale = size ? ` font-size: ${size} !important; line-height: 1;` : ''
      // `-webkit-text-stroke` inherits, so every class states its own width --
      // `0` included. Without that, a face nested inside a stroked one (the
      // Vivaldi signature inside the Corvisa letter) would wear the parent's
      // stroke. `currentColor` keeps it with the text wherever it is drawn.
      const weight = ` -webkit-text-stroke: ${stroke ?? '0'} currentColor;`
      // `.basic` means "not flavor text", so it opts out of the stroke too.
      const plain = stroke
        ? `\n.${className} .basic, .${className} p.basic { -webkit-text-stroke-width: 0; }`
        : ''
      return (
        `@font-face { font-family: "${family}"; src: url("${url}"); font-display: swap; }\n` +
        // The flavor-text components set the font on every `:deep(p)` they
        // render, which beats a family inherited from an ancestor -- hence the
        // descendant rule and `!important`. `.basic` keeps its own `!important`
        // reset, so a `<p class='basic'>` inside stays in the plain UI font.
        `.${className}, .${className} p { font-family: "${family}" !important;${scale}${weight} }` +
        plain
      )
    })
    .join('\n')
}

if (typeof document !== 'undefined' && homebrewFonts.length > 0) {
  const style = document.createElement('style')
  style.dataset.homebrewFonts = ''
  style.textContent = homebrewFontCss()
  document.head.append(style)
}
