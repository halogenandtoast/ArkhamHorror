# AGENTS instructions for ArkhamHorror

This repository contains a Haskell backend (`backend/`) and a Vue/TypeScript frontend (`frontend/`).

## Formatting

* Haskell source must be formatted with **fourmolu** using the configuration at `backend/fourmolu.yaml`. Run `fourmolu -i <file>` before committing.
* TypeScript/Vue files should be formatted with **prettier** using the defaults configured by the project. Run `npx prettier -w <file>` if you modify frontend sources.

## Testing

* After modifying backend code, run `stack test` inside the `backend` directory.
* After modifying frontend code, run `npm run build` from `frontend` to ensure the project still compiles.

## Adding new game content

* Haskell modules for cards, scenarios, and other game items live under `backend/arkham-api/library/Arkham`. Templates for creating these modules are defined in `backend/.projections.json`.
* Files representing game content should use CamelCase with no spaces in the name.
  * Card files follow these conventions using "Foo Bar" as an example:
    * Card with only a title -> `FooBar`
    * Card with a subtitle -> `FooBarSubtitle`
    * Duplicate name within the same set -> `FooBar_123` (where `123` is the card number)
    * Same name across different sets -> `FooBarSetName`
* Folder structure for common card types under `backend/arkham-api/library/Arkham`:
  * Acts -> `Act/Cards`
  * Agendas -> `Agenda/Cards`
  * Assets -> `Asset/Cards`
  * Enemies -> `Enemy/Cards`
  * Events -> `Event/Cards`
  * Locations -> `Location/Cards`
  * Scenarios -> `Scenario/Scenarios`
  * Skills -> `Skill/Cards`
  * Stories -> `Story/Cards`
  * Treacheries -> `Treachery/Cards`
  * Encounter-related helpers live in `EncounterCard` and `EncounterSet` modules.
* Each of these folders contains a `Cards.hs` file that indexes the available cards for that type.
* Revealed and unrevealed locations are managed in `backend/arkham-api/library/Arkham/Location/Cards.hs`.
* Reference files using the latest code and API:
  * **Locations**:
    * `backend/arkham-api/library/Arkham/Location/Cards/EsotericOrderOfDagon.hs`
    * `backend/arkham-api/library/Arkham/Location/Cards/NewChurchGreen.hs`
    * `backend/arkham-api/library/Arkham/Location/Cards/UnderwaterCavern.hs`
  * **Enemies**:
    * `backend/arkham-api/library/Arkham/Enemy/Cards/RobertFriendlyDisgruntledDockworker.hs`
    * `backend/arkham-api/library/Arkham/Enemy/Cards/OtheraGilmanProprietessOfTheHotel.hs`
    * `backend/arkham-api/library/Arkham/Enemy/Cards/PriestOfDagon.hs`
    * `backend/arkham-api/library/Arkham/Enemy/Cards/DeepOneBull.hs`
  * **Story Assets**:
    * `backend/arkham-api/library/Arkham/Asset/Assets/ThomasDawsonSoldierInANewWar.hs`
    * `backend/arkham-api/library/Arkham/Asset/Assets/ElinaHarperKnowsTooMuch.hs`
    * `backend/arkham-api/library/Arkham/Asset/Assets/ElinaHarpersCarRunning.hs`
    * `backend/arkham-api/library/Arkham/Asset/Assets/ElinaHarpersCarStopped.hs`
  * **Treachery**:
    * `backend/arkham-api/library/Arkham/Treachery/Cards/PsychicPull.hs`
    * `backend/arkham-api/library/Arkham/Treachery/Cards/TidalAlignment.hs`
    * `backend/arkham-api/library/Arkham/Treachery/Cards/Syzygy.hs`
  * **Story**:
    * `backend/arkham-api/library/Arkham/Story/Cards/BaseCamp.hs`
    * `backend/arkham-api/library/Arkham/Story/Cards/Captured.hs`
  * **Scenario Setup**:
    * `backend/arkham-api/library/Arkham/Scenario/Scenarios/TheVanishingOfElinaHarper.hs`
    * `backend/arkham-api/library/Arkham/Scenario/Scenarios/IceAndDeathPart1.hs`
  * **Campaign Steps**:
    * `backend/arkham-api/library/Arkham/Campaigns/TheInnsmouthConspiracy/CampaignSteps.hs`
  * **Chaos Bag settings**:
    * `backend/arkham-api/library/Arkham/Campaigns/TheInnsmouthConspiracy/ChaosBag.hs`
  * **Campaign Wide Helpers**:
    * `backend/arkham-api/library/Arkham/Campaigns/TheInnsmouthConspiracy/Helpers.hs`
    * `backend/arkham-api/library/Arkham/Campaigns/EdgeOfTheEarth/Helpers.hs`
  * **Flavor Text**:
    * `backend/arkham-api/library/Arkham/Campaigns/TheDreamEaters/FlavorText.hs`

## Card module conventions

The reference files above follow a very consistent layout. When adding new
cards or scenarios, keep these patterns in mind:

* Modules live under `Arkham/<Type>/Cards` (or `Arkham/Asset/Assets` for story
  assets). The module name matches the path, for example:

  ```haskell
  module Arkham.Location.Cards.EsotericOrderOfDagon
    (esotericOrderOfDagon, EsotericOrderOfDagon(..))
  where
  ```

* Import the card index for that type as `Cards` and the lifted import module
  for helpers:

  ```haskell
  import Arkham.Location.Cards qualified as Cards
  import Arkham.Location.Import.Lifted
  ```

* Define a `newtype` wrapping the appropriate attribute record and derive the
  standard classes (e.g. `IsLocation`, `HasModifiersFor`). Provide a builder
  function using helpers like `location`, `enemy`, `ally`, or `treachery`.

* Implement `HasAbilities` and `RunMessage` instances using the lifted message
  utilities shown in the examples.

* Register the new card in the relevant `Cards.hs` file so it can be looked up
  by card code.

* Scenario modules live in `Scenario/Scenarios` and usually have a companion
  `Helpers.hs` under `Scenarios/<ScenarioName>`. Campaign-wide utilities are
  placed in `Campaigns/<CampaignName>/Helpers.hs`.

## Pull Request guidelines

* Keep commits focused and descriptive.
* In the PR summary list any commands run and indicate whether they succeeded.
* Do not commit build artefacts or files ignored by `.gitignore`.
