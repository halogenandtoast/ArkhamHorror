module Arkham.Act.Cards.StrangeInfestation (strangeInfestation) where

import Arkham.Ability
import Arkham.Card
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.EnemyLocation.Cards qualified as EnemyLocations
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Grid (GridLocation (..), Pos (..))
import Arkham.Matcher
import Arkham.Scenarios.HemlockHouse.Helpers (locationIsUnsealed)
import Arkham.Token (Token (..))
import Arkham.Trait (Trait (Dormant))

newtype StrangeInfestation = StrangeInfestation ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeInfestation :: ActCard StrangeInfestation
strangeInfestation = act (1, A) StrangeInfestation Cards.strangeInfestation Nothing

instance HasModifiersFor StrangeInfestation where
  getModifiersFor (StrangeInfestation a) = do
    -- "Clues cannot be discovered from locations with no investigators."
    modifySelect a Anyone [CannotDiscoverCluesAt $ not_ $ LocationWithInvestigator Anyone]

instance HasAbilities StrangeInfestation where
  getAbilities (StrangeInfestation a) =
    [ -- "[action] If your location is unsealed and [[Dormant]], investigators
      -- at that location spend 1 [per_investigator] clues, as a group: Place 1
      -- resource on it, as a seal."
      restricted
        a
        1
        (exists $ YourLocation <> LocationWithTrait Dormant)
        $ actionAbilityWithCost (SameLocationGroupClueCost (PerPlayer 1) YourLocation)
    , -- "Objective - At the end of the round, if a total of 7 locations are
      -- sealed and/or in the victory display, advance."
      restricted
        a
        2
        ( HasCalculation
            ( SumCalculation
                [ CountLocations (LocationWithToken Resource)
                , VictoryDisplayCountCalculation (basic $ CardWithType EnemyLocationCardType)
                ]
            )
            (atLeast 7)
        )
        $ Objective
        $ forced (RoundEnds #when)
    ]

instance RunMessage StrangeInfestation where
  runMessage msg a@(StrangeInfestation attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      mlid <- getMaybeLocation iid
      for_ mlid $ \lid -> do
        unsealed <- locationIsUnsealed lid
        when unsealed
          $ push
          $ PlaceTokens (toSource (attrs.ability 1)) (toTarget lid) Resource 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- Strange Infestation → The Heart of the House. Bring the Shapeless
      -- Cellar into play at floor 0 (the bottom of the middle column).
      cellarCard <- genCard EnemyLocations.shapelessCellar
      cellarLid <- LocationId <$> getRandom
      pushAll
        [ PlaceEnemyLocation cellarLid cellarCard
        , PlaceGrid (GridLocation (Pos 0 (-1)) cellarLid)
        ]
      advanceActDeck attrs
      pure a
    _ -> StrangeInfestation <$> liftRunMessage msg attrs
