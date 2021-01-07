import { JsonDecoder } from 'ts.data.json';

export interface Scenario {
  tag: string;
  contents: ScenarioContents;
}

export interface ScenarioDeck {
  tag: string;
}

export interface ScenarioContents {
  name: string;
  id: string;
  difficulty: string;
  locationLayout: string[] | null;
  deck: ScenarioDeck | null;
}

export const scenarioDeckDecoder = JsonDecoder.object<ScenarioDeck>({
  tag: JsonDecoder.string,
}, 'ScenarioDeck');

export const scenarioContentsDecoder = JsonDecoder.object<ScenarioContents>({
  name: JsonDecoder.string,
  id: JsonDecoder.string,
  difficulty: JsonDecoder.string,
  locationLayout: JsonDecoder.nullable(JsonDecoder.array<string>(JsonDecoder.string, 'GridLayout[]')),
  deck: JsonDecoder.nullable(scenarioDeckDecoder),
}, 'ScenarioContents');

export const scenarioDecoder = JsonDecoder.object<Scenario>({
  tag: JsonDecoder.string,
  contents: scenarioContentsDecoder,
}, 'Scenario');
