export type Gateable = { alpha?: boolean; beta?: boolean; dev?: boolean }

export type DisplayRuleOptions = {
  alpha: boolean
  beta: boolean
  dev: boolean
}

export const isDevBuild = () => !import.meta.env.PROD

export const shouldDisplay = (item: Gateable, { alpha, beta, dev }: DisplayRuleOptions) => {
  if (item.dev) return dev && alpha
  if (item.beta) return beta
  if (item.alpha) return alpha
  return true
}

export const filterDisplayable = <T extends Gateable>(items: T[], options: DisplayRuleOptions) =>
  items.filter((item) => shouldDisplay(item, options))
