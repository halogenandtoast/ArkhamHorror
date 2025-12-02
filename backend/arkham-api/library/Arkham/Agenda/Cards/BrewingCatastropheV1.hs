module Arkham.Agenda.Cards.BrewingCatastropheV1 (brewingCatastropheV1) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (AssetDefeated)
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach, modifySelect, modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Scenarios.DogsOfWar.Helpers

newtype BrewingCatastropheV1 = BrewingCatastropheV1 AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brewingCatastropheV1 :: AgendaCard BrewingCatastropheV1
brewingCatastropheV1 = agenda (1, A) BrewingCatastropheV1 Cards.brewingCatastropheV1 (Static 12)

instance HasModifiersFor BrewingCatastropheV1 where
  getModifiersFor (BrewingCatastropheV1 a) = do
    modifySelf a [OtherDoomSubtracts]
    modifySelect a AnyEnemy [RemoveKeyword #hunter, AddKeyword (Keyword.Patrol locationWithKeyLocus)]
    ok <-
      select
        $ LocationWithAsset
        $ mapOneOf assetIs [Assets.keyLocusLastBastion, Assets.keyLocusDefensiveBarrier]
    if null ok
      then do
        flip (modifySelect a) [KeyLocusLocation]
          $ LocationWithAsset
          $ assetIs Assets.theClaretKnightHerSwornChampion
        modifySelect a (assetIs Assets.theClaretKnightHerSwornChampion) [IsKeyLocus]
      else modifyEach a ok [KeyLocusLocation]

instance HasAbilities BrewingCatastropheV1 where
  getAbilities (BrewingCatastropheV1 a) =
    [ mkAbility a 1
        $ Objective
        $ forced
        $ AssetDefeated #when ByAny (assetIs Assets.theClaretKnightHerSwornChampion)
    ]

instance RunMessage BrewingCatastropheV1 where
  runMessage msg a@(BrewingCatastropheV1 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceAgenda attrs
      pure a
    AdvanceAgendaBy (isSide B attrs -> True) means -> do
      case means of
        AgendaAdvancedWithOther -> push R3
        _ -> advanceCurrentAct attrs
      pure a
    _ -> BrewingCatastropheV1 <$> liftRunMessage msg attrs
