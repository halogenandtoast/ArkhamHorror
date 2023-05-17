import { JsonDecoder } from 'ts.data.json';
import { Placement, placementDecoder } from '@/arkham/types/Placement';

export interface Story {
  id: string
  placement: Placement
}

export const storyDecoder = JsonDecoder.object<Story>({
  id: JsonDecoder.string,
  placement: placementDecoder,
}, 'Story');
