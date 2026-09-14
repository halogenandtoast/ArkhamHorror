import type { Directive } from 'vue'

/* Focus a field as soon as it appears.
 *
 * The `autofocus` attribute is honoured once, while the document is first
 * loading. Anything inserted after that -- a menu opening, a field swapping to
 * its search box -- lands in a document that has already processed its autofocus
 * candidates, so the attribute does nothing at all and the caret stays wherever
 * it was. Every search in the builder wants the same thing, so it is a directive
 * rather than a ref and a nextTick in each of them.
 */
export const vFocus: Directive<HTMLElement> = {
  mounted: (el) => el.focus(),
}
