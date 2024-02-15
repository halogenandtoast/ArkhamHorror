module Arkham.Location.Cards.Basement (
  basement,
  Basement (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Name
import Arkham.Projection
import Arkham.Trait (Trait (Humanoid))

newtype Basement = Basement LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

basement :: LocationCard Basement
basement = location Basement Cards.basement 4 (PerPlayer 1)

instance HasAbilities Basement where
  getAbilities (Basement attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility attrs 1 Here actionAbility
      , restrictedAbility
          attrs
          2
          ( exists (assetIs Assets.tomeOfRituals)
              <> exists (investigatorAt (toId attrs) <> InvestigatorWithAnyClues)
          )
          $ ReactionAbility (EnemyDefeated #after You ByAny (enemyAt $ toId attrs)) Free
      ]

instance RunMessage Basement where
  runMessage msg l@(Basement attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      humanoids <- select $ EnemyWithTrait Humanoid <> NotEnemy (enemyAt $ toId attrs)
      when (notNull humanoids) $ do
        player <- getPlayer iid
        push
          $ chooseOrRunOne
            player
            [ targetLabel humanoid [MoveToward (toTarget humanoid) (LocationWithId $ toId attrs)]
            | humanoid <- humanoids
            ]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      player <- getPlayer iid
      iids <- selectWithField InvestigatorClues $ investigatorAt (toId attrs) <> InvestigatorWithAnyClues
      tomeOfRituals <- selectOne $ assetIs Assets.tomeOfRituals

      unless (null iids || isNothing tomeOfRituals) $ do
        named <- traverse (\(iid', x) -> (,x) <$> field InvestigatorName iid') iids
        push
          $ chooseAmounts
            player
            "number of clues to move to Tome of Rituals"
            (MinAmountTarget 0)
            (map (\(name, x) -> (toTitle name, (0, x))) named)
            (toTarget attrs)
      pure l
    ResolveAmounts _ choices (isTarget attrs -> True) -> do
      named <- selectWithField InvestigatorName UneliminatedInvestigator
      let
        iidsWithAmounts =
          flip mapMaybe named $ \(iid', name) ->
            let amount = getChoiceAmount (toTitle name) choices
             in guard (amount > 0) $> (iid', amount)
      tomeOfRituals <- selectJust $ assetIs Assets.tomeOfRituals
      pushAll
        $ [ MovedClues (toSource iid) (toTarget tomeOfRituals) n
          | (iid, n) <- iidsWithAmounts
          ]
      pure l
    _ -> Basement <$> runMessage msg attrs
