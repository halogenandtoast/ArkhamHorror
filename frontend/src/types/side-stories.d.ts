declare module '@/arkham/data/side-stories' {
  import type { Scenario } from '@/arkham/data'
  const value: (Scenario & { dev?: boolean })[]
  export default value
}
