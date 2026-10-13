/* Getting an image ready to upload for a custom card or the set it belongs to.
 *
 * Shared because the card form, a set's expansion icon and a set's token art
 * all upload to the same endpoint under the same 1MB cap, and all three are
 * images somebody dragged out of a folder at whatever size they happened to be.
 */

/* What the server accepts (`maxArtBytes`). Checked here as well so an image too
 * big is stepped down rather than refused after the round trip. */
export const MAX_ART_BYTES = 1024 * 1024

/* Downscaled before upload: the art is served to every player who sees the
 * card, and a full-resolution scan has no business going up.
 *
 * `maxWidth` is the one thing that differs between uses -- a card face wants
 * 500px, a token marker is drawn at about 25 -- so it is the caller's to say.
 */
export async function readImage(file: File | Blob, maxWidth = 500): Promise<Blob> {
  const dataUrl: string = await new Promise((resolve, reject) => {
    const reader = new FileReader()
    reader.onload = () => resolve(reader.result as string)
    reader.onerror = () => reject(reader.error)
    reader.readAsDataURL(file)
  })

  const image = new Image()
  await new Promise((resolve, reject) => {
    image.onload = resolve
    image.onerror = reject
    image.src = dataUrl
  })

  const scale = Math.min(1, maxWidth / image.width)
  const canvas = document.createElement('canvas')
  canvas.width = Math.round(image.width * scale)
  canvas.height = Math.round(image.height * scale)
  canvas.getContext('2d')?.drawImage(image, 0, 0, canvas.width, canvas.height)

  /* The server caps art at 1MB. A few hundred pixels of WEBP is well under
   * that, but step the quality down rather than fail on an image that happens
   * not to be. */
  for (const quality of [0.85, 0.7, 0.55, 0.4]) {
    const blob: Blob | null = await new Promise((resolve) =>
      canvas.toBlob(resolve, 'image/webp', quality),
    )
    if (blob && blob.size <= MAX_ART_BYTES) return blob
  }

  throw new Error('Image is too large')
}
