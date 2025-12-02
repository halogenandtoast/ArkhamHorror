module Arkham.Agenda.Cards.TheTrueCulpritV8 (theTrueCulpritV8) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Name
import Arkham.Trait (Trait (Staff))
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheTrueCulpritV8 = TheTrueCulpritV8 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV8 :: AgendaCard TheTrueCulpritV8
theTrueCulpritV8 = agenda (3, A) TheTrueCulpritV8 Cards.theTrueCulpritV8 (Static 6)

instance HasAbilities TheTrueCulpritV8 where
  getAbilities (TheTrueCulpritV8 attrs) =
    guard (onSide A attrs)
      *> ( [ controlled_ (proxied (assetIs asset) attrs) 1
               $ actionAbilityWithCost (AssetClueCost (toTitle asset) (assetIs asset) $ Static 1)
           | asset <-
               [ Cards.alienDevice
               , Cards.managersKey
               , Cards.sinisterSolution
               , Cards.timeWornLocket
               , Cards.tomeOfRituals
               ]
           ]
             <> [ skipForAll
                    $ groupLimit PerTestOrAbility
                    $ mkAbility attrs 2
                    $ freeReaction (EnemyDefeated #after Anyone ByAny $ EnemyWithTrait Staff)
                , restricted
                    attrs
                    3
                    ( exists
                        $ treacheryIs Treacheries.harvestedBrain
                        <> TreacheryWithHorror (AtLeast $ StaticWithPerPlayer 2 1)
                    )
                    $ Objective
                    $ forced AnyWindow
                ]
         )

instance RunMessage TheTrueCulpritV8 where
  runMessage msg a@(TheTrueCulpritV8 attrs) = runQueueT $ case msg of
    UseThisAbility iid (ProxySource _ (isSource attrs -> True)) 1 -> do
      findAndDrawEncounterCard iid $ #enemy <> CardWithTrait Staff
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      harvestedBrain <- selectJust $ treacheryIs Treacheries.harvestedBrain
      placeTokens (attrs.ability 2) harvestedBrain #horror 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advanceAgenda attrs
      pure a
    AdvanceAgendaBy (isSide B attrs -> True) means -> do
      push $ if means == AgendaAdvancedWithDoom then R2 else R1
      pure a
    _ -> TheTrueCulpritV8 <$> liftRunMessage msg attrs
