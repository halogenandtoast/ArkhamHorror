module Arkham.Location.Cards.OfficeMurderAtTheExcelsiorHotel (
  officeMurderAtTheExcelsiorHotel,
  OfficeMurderAtTheExcelsiorHotel (..),
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

newtype OfficeMurderAtTheExcelsiorHotel = OfficeMurderAtTheExcelsiorHotel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

officeMurderAtTheExcelsiorHotel :: LocationCard OfficeMurderAtTheExcelsiorHotel
officeMurderAtTheExcelsiorHotel = location OfficeMurderAtTheExcelsiorHotel Cards.officeMurderAtTheExcelsiorHotel 3 (PerPlayer 2)

instance HasAbilities OfficeMurderAtTheExcelsiorHotel where
  getAbilities (OfficeMurderAtTheExcelsiorHotel attrs) =
    withRevealedAbilities
      attrs
      [ withTooltip
          "{action}: Test {intellect} (0). For each point you succeed by, you may move 1 clue controlled by an investigator in the Office to Manager's key (if it is in play)."
          $ restrictedAbility attrs 1 Here actionAbility
      ]

instance RunMessage OfficeMurderAtTheExcelsiorHotel where
  runMessage msg l@(OfficeMurderAtTheExcelsiorHotel attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ beginSkillTest iid (toAbilitySource attrs 1) iid #intellect 0
      pure l
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      player <- getPlayer iid
      iids <- selectWithField InvestigatorClues $ investigatorAt (toId attrs) <> InvestigatorWithAnyClues

      unless (null iids) $ do
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
        $ [ MovedClues (toSource iid) (toTarget managersKey) n
          | (iid, n) <- iidsWithAmounts
          ]
      pure l
    _ -> OfficeMurderAtTheExcelsiorHotel <$> runMessage msg attrs
