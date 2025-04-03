import * as JsonDecoder from "ts.data.json";

export type Difficulty = 'Easy' | 'Standard' | 'Hard' | 'Expert';

export const difficultyDecoder = JsonDecoder.oneOf<Difficulty>([
    JsonDecoder.literal('Easy'),
    JsonDecoder.literal('Standard'),
    JsonDecoder.literal('Hard'),
    JsonDecoder.literal('Expert')
], 'Difficulty');
