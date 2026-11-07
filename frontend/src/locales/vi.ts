import base from '@/locales/vi/base.json'
import log from '@/locales/vi/log.json'
import label from '@/locales/vi/label.json'
import gameBoard from '@/locales/vi/gameBoard/gameBoard'
import theFeastOfHemlockVale from '@/locales/vi/theFeastOfHemlockVale'

// Partial translation: anything missing falls back to English (fallbackLocale in main.ts).
export default { ...base, ...label, ...gameBoard, log, theFeastOfHemlockVale }
