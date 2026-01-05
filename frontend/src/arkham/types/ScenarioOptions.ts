import * as JsonDecoder from 'ts.data.json';

export type ScenarioOptions = {
  scenarioOptionsStandalone: boolean
  scenarioOptionsPerformTarotReading: boolean
  scenarioOptionsLeadInvestigator: string | null
  scenarioOptionsSkipInvestigatorSetup: boolean
}

export const scenarioOptionsDecoder = JsonDecoder.object<ScenarioOptions>(
  {
    scenarioOptionsStandalone: JsonDecoder.boolean(),
    scenarioOptionsPerformTarotReading: JsonDecoder.boolean(),
    scenarioOptionsLeadInvestigator: JsonDecoder.nullable(JsonDecoder.string()),
    scenarioOptionsSkipInvestigatorSetup: JsonDecoder.boolean(),
  },
  'ScenarioOptions',
);

export const defaultScenarioOptions: ScenarioOptions = {
  scenarioOptionsStandalone: false,
  scenarioOptionsPerformTarotReading: false,
  scenarioOptionsLeadInvestigator: null,
  scenarioOptionsSkipInvestigatorSetup: false,
};
