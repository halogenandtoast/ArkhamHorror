/* A card set's link: where it lives in the world -- the post announcing it, the
 * thread it is discussed in -- which is nearly always a better read than any
 * blurb that fits on a listing.
 *
 * The server stores only http addresses and refuses anything else, so what
 * comes back is safe to put in an anchor. The two things here are about showing
 * it and about the one way saving one fails. */

/* The host, which is what tells a reader where they are being sent. The whole
 * address is a line of slugs and dates nobody reads, and the anchor carries it
 * anyway for anyone who hovers. */
export function setLinkLabel(url: string): string {
  try {
    return new URL(url).host.replace(/^www\./, '')
  } catch {
    // Stored links are http addresses, so this is unreachable short of a
    // hand-edited row -- and showing it raw beats showing nothing.
    return url
  }
}

/* The server's one complaint about a set's link worth repeating as itself: a
 * flat "could not save" for an address typed without a scheme sends somebody
 * looking for a fault that is in front of them. */
export function isBadLink(e: unknown): boolean {
  return (e as { response?: { status?: number } })?.response?.status === 400
}
