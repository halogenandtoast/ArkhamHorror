module Arkham.Location.Cards.TempleOfTheMoonLizard (
  templeOfTheMoonLizard,
  TempleOfTheMoonLizard (..),
)
where

import Arkham.Card
import Arkham.Discard
import Arkham.GameValue
import Arkham.Helpers.Message qualified as Msg
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner hiding (beginSkillTest, chooseOrRunOne)
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers
import Arkham.Window (Window (windowType))
import Arkham.Window qualified as Window

newtype Meta = Meta {hasUsedSuccess :: [InvestigatorId]}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype TempleOfTheMoonLizard = TempleOfTheMoonLizard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templeOfTheMoonLizard :: LocationCard TempleOfTheMoonLizard
templeOfTheMoonLizard =
  locationWith TempleOfTheMoonLizard Cards.templeOfTheMoonLizard 3 (PerPlayer 1) (setMeta $ Meta [])

instance HasAbilities TempleOfTheMoonLizard where
  getAbilities (TempleOfTheMoonLizard attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 Here actionAbility
      , restrictedAbility attrs 2 Here $ forced $ DiscoverClues #after You (be attrs) AnyValue
      ]

getClues :: [Window] -> Int
getClues ((windowType -> Window.DiscoverClues _ _ _ n) : rest) = n + getClues rest
getClues (_ : rest) = getClues rest
getClues [] = 0

instance RunMessage TempleOfTheMoonLizard where
  runMessage msg l@(TempleOfTheMoonLizard attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      beginSkillTest iid (attrs.ability 1) iid #willpower (Fixed 5)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      let meta = toResult @Meta attrs.meta
      if iid `elem` hasUsedSuccess meta
        then pure l
        else do
          reduceAlarmLevel (attrs.ability 1) iid
          pure
            $ TempleOfTheMoonLizard
            $ attrs
            & metaL
            .~ toJSON (meta {hasUsedSuccess = iid : hasUsedSuccess meta})
    UseCardAbility iid (isSource attrs -> True) 2 (getClues -> n) _ -> do
      discardableCards <- fieldMap InvestigatorHand (count (`cardMatch` DiscardableCard)) iid
      chooseOrRunOne iid
        $ [Label ("Take " <> tshow n <> " horror") [Msg.assignHorror iid (attrs.ability 2) n]]
        <> [ Label
            ("Discard " <> tshow n <> " cards")
            [toMessage $ discardFromHand iid (attrs.ability 2) DiscardChoose n]
           | discardableCards > 0
           ]
      pure l
    _ -> TempleOfTheMoonLizard <$> liftRunMessage msg attrs
