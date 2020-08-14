import { JsonDecoder } from 'ts.data.json';

export interface Scenario {
  tag: string;
  contents: ScenarioContents;
}

export interface ScenarioContents {
  name: string;
  id: string;
  locationLayout: string[] | null;
}

export const scenarioContentsDecoder = JsonDecoder.object<ScenarioContents>({
  name: JsonDecoder.string,
  id: JsonDecoder.string,
  locationLayout: JsonDecoder.nullable(JsonDecoder.array<string>(JsonDecoder.string, 'GridLayout[]')),
}, 'ScenarioContents');

export const scenarioDecoder = JsonDecoder.object<Scenario>({
  tag: JsonDecoder.string,
  contents: scenarioContentsDecoder,
}, 'Scenario');
