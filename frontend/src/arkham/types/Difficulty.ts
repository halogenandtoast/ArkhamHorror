import { JsonDecoder } from "ts.data.json";

export type Difficulty = 'Easy' | 'Standard' | 'Hard' | 'Expert';

export const difficultyDecoder = JsonDecoder.oneOf<Difficulty>([
    JsonDecoder.isExactly('Easy'),
    JsonDecoder.isExactly('Standard'),
    JsonDecoder.isExactly('Hard'),
    JsonDecoder.isExactly('Expert')
], 'Difficulty');