module Arkham.Location.Cards.Room212 (
  room212,
  Room212 (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Movement
import Arkham.Name
import Arkham.Projection

newtype Room212 = Room212 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

room212 :: LocationCard Room212
room212 = locationWith Room212 Cards.room212 4 (PerPlayer 2) (labelL .~ "room212")

instance HasModifiersFor Room212 where
  getModifiersFor target (Room212 attrs) | attrs `is` target = do
    pure $ toModifiers attrs [Blocked | unrevealed attrs]
  getModifiersFor _ _ = pure []

instance HasAbilities Room212 where
  getAbilities (Room212 attrs) =
    if unrevealed attrs
      then
        withBaseAbilities
          attrs
          [ withTooltip
              "{action}: Test {agility} (4) to attempt to pick the lock. If you succeed, reveal Room 212 and immediately move to it."
              $ restrictedAbility
                (proxied (LocationMatcherSource "Second Floor Hall") attrs)
                1
                (OnLocation "Second Floor Hall")
                #action
          ]
      else
        withRevealedAbilities
          attrs
          [ withTooltip
              "{action}: Test {intellect} (3). If you succeed, move any number of clues controlled by investigators at this location to Sinister Solution (if it is in play)."
              $ restrictedAbility attrs 1 Here actionAbility
          ]

instance RunMessage Room212 where
  runMessage msg l@(Room212 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ beginSkillTest iid (attrs.ability 1) iid #intellect (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      player <- getPlayer iid
      iids <- selectWithField InvestigatorClues $ investigatorAt (toId attrs) <> InvestigatorWithAnyClues
      sinisterSolution <- selectOne $ assetIs Assets.sinisterSolution

      unless (null iids || isNothing sinisterSolution) $ do
        named <- traverse (\(iid', n) -> (,n) <$> field InvestigatorName iid') iids
        push
          $ chooseAmounts
            player
            "number of clues to move to Sinister Solution"
            (MinAmountTarget 0)
            (map (\(name, n) -> (toTitle name, (0, n))) named)
            (toTarget attrs)
      pure l
    ResolveAmounts _ choices (isTarget attrs -> True) -> do
      named <- selectWithField InvestigatorName UneliminatedInvestigator
      let
        iidsWithAmounts =
          flip mapMaybe named $ \(iid', name) ->
            let amount = getChoiceAmount (toTitle name) choices
             in guard (amount > 0) $> (iid', amount)
      sinisterSolution <- selectJust $ assetIs Assets.sinisterSolution
      pushAll
        $ [ MovedClues (attrs.ability 1) (toSource iid) (toTarget sinisterSolution) n
          | (iid, n) <- iidsWithAmounts
          ]
      pure l
    UseThisAbility iid p@(ProxySource _ (isSource attrs -> True)) 1 -> do
      push $ beginSkillTest iid (toAbilitySource p 1) iid #agility (Fixed 4)
      pure l
    PassedThisSkillTest iid source@(AbilitySource (ProxySource _ (isSource attrs -> True)) 1) -> do
      pushAll [Msg.RevealLocation Nothing (toId attrs), Move $ move source iid (toId attrs)]
      pure l
    _ -> Room212 <$> runMessage msg attrs
