import { JsonDecoder } from 'ts.data.json';

export interface Scenario {
  tag: string;
  contents: ScenarioContents;
}

export interface ScenarioContents {
  name: string;
  id: string;
}

export const scenarioContentsDecoder = JsonDecoder.object<ScenarioContents>({
  name: JsonDecoder.string,
  id: JsonDecoder.string,
}, 'ScenarioContents');

export const scenarioDecoder = JsonDecoder.object<Scenario>({
  tag: JsonDecoder.string,
  contents: scenarioContentsDecoder,
}, 'Scenario');
