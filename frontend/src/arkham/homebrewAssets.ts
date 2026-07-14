// Frontend assets contributed by homebrew campaigns, discovered from the
// frontend/homebrew/<campaign>/ directories:
//
// - `*.css` is injected into the bundle as-is (asset URLs inside should be
//   absolute, e.g. /img/arkham/homebrew/<campaign>/...).
// - `icons.json` maps icon keys to CSS classes; each entry hooks the text
//   formatters so `{key}` (i18n/flavor text) and `[key]` (ArkhamDB-style card
//   text) both render as `<span class="<class>"></span>`.
//
// Like the locale and instance discovery, dropping a campaign directory in
// requires no registration here.
import.meta.glob('@homebrew/*/*.css', { eager: true })

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
