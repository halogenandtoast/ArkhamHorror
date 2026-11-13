import { useDbCardStore } from '@/stores/dbCards'

/* Investigators reach the client with their English card title (`name.title`).
 * Outside English the printed name comes from the localized card database, looked
 * up among investigator cards so an asset or enemy sharing the name cannot answer;
 * a title the database does not know stays as it is. */
export function localizedInvestigatorName(title: string): string {
  if ((localStorage.getItem('language') || 'en') === 'en') return title
  return useDbCardStore().getCardName(title, 'investigator')
}
