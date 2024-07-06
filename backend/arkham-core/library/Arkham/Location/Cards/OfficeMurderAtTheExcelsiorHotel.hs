module Arkham.Location.Cards.OfficeMurderAtTheExcelsiorHotel (
  officeMurderAtTheExcelsiorHotel,
  OfficeMurderAtTheExcelsiorHotel (..),
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

newtype OfficeMurderAtTheExcelsiorHotel = OfficeMurderAtTheExcelsiorHotel LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

officeMurderAtTheExcelsiorHotel :: LocationCard OfficeMurderAtTheExcelsiorHotel
officeMurderAtTheExcelsiorHotel = location OfficeMurderAtTheExcelsiorHotel Cards.officeMurderAtTheExcelsiorHotel 3 (PerPlayer 2)

instance HasModifiersFor OfficeMurderAtTheExcelsiorHotel where
  getModifiersFor (InvestigatorTarget iid) (OfficeMurderAtTheExcelsiorHotel attrs) = do
    hasManagersKey <- iid <=~> HasMatchingAsset (assetIs Assets.managersKey)
    pure $ toModifiers attrs [CannotEnter (toId attrs) | unrevealed attrs && not hasManagersKey]
  getModifiersFor _ _ = pure []

instance HasAbilities OfficeMurderAtTheExcelsiorHotel where
  getAbilities (OfficeMurderAtTheExcelsiorHotel attrs) =
    if unrevealed attrs
      then
        withBaseAbilities
          attrs
          [ withTooltip
              "{action}: Test {agility} (3) to attempt to pick the lock. If you succeed, reveal Office and immediately move to it."
              $ restrictedAbility
                (proxied (LocationMatcherSource "Basement") attrs)
                1
                (OnLocation "Basement")
                #action
          ]
      else
        withRevealedAbilities
          attrs
          [ withTooltip
              "{action}: Test {intellect} (0). For each point you succeed by, you may move 1 clue controlled by an investigator in the Office to Manager's key (if it is in play)."
              $ restrictedAbility attrs 1 Here actionAbility
          ]

instance RunMessage OfficeMurderAtTheExcelsiorHotel where
  runMessage msg l@(OfficeMurderAtTheExcelsiorHotel attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ beginSkillTest iid (toAbilitySource attrs 1) iid #intellect (Fixed 0)
      pure l
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      player <- getPlayer iid
      iids <- selectWithField InvestigatorClues $ investigatorAt (toId attrs) <> InvestigatorWithAnyClues
      managersKey <- selectOne $ assetIs Assets.managersKey

      unless (null iids || isNothing managersKey) $ do
        named <- traverse (\(iid', x) -> (,x) <$> field InvestigatorName iid') iids
        push
          $ chooseAmounts
            player
            "number of clues to move to Alien Device"
            (MaxAmountTarget n)
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
      managersKey <- selectJust $ assetIs Assets.managersKey
      pushAll
        $ [ MovedClues (attrs.ability 1) (toSource iid) (toTarget managersKey) n
          | (iid, n) <- iidsWithAmounts
          ]
      pure l
    UseThisAbility iid p@(ProxySource _ (isSource attrs -> True)) 1 -> do
      push $ beginSkillTest iid (toAbilitySource p 1) iid #agility (Fixed 3)
      pure l
    PassedThisSkillTest iid source@(AbilitySource (ProxySource _ (isSource attrs -> True)) 1) -> do
      pushAll [Msg.RevealLocation Nothing (toId attrs), Move $ move source iid (toId attrs)]
      pure l
    _ -> OfficeMurderAtTheExcelsiorHotel <$> runMessage msg attrs
