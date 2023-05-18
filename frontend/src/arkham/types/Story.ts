import { JsonDecoder } from 'ts.data.json';
import { Placement, placementDecoder } from '@/arkham/types/Placement';
import { Target, targetDecoder } from '@/arkham/types/Target';

export interface Story {
  id: string
  placement: Placement
  otherSide: Target | null
}

export const storyDecoder = JsonDecoder.object<Story>({
  id: JsonDecoder.string,
  placement: placementDecoder,
  otherSide: JsonDecoder.nullable(targetDecoder)
}, 'Story');
