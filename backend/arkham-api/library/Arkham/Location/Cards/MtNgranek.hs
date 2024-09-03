module Arkham.Location.Cards.MtNgranek (mtNgranek, MtNgranek (..)) where

import Arkham.GameValue
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Story

newtype MtNgranek = MtNgranek LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mtNgranek :: LocationCard MtNgranek
mtNgranek = location MtNgranek Cards.mtNgranek 3 (PerPlayer 1)

instance HasAbilities MtNgranek where
  getAbilities (MtNgranek attrs) =
    veiled
      attrs
      [ skillTestAbility
          $ restrictedAbility attrs 1 (not_ $ Remembered ObtainedSuppliesFromBaharna)
          $ forced
          $ oneOf [Enters #after You (be attrs), DiscoverClues #after You (be attrs) AnyValue]
      ]

instance RunMessage MtNgranek where
  runMessage msg l@(MtNgranek attrs) = case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.theLikenessOfOld
      pure . MtNgranek $ attrs & canBeFlippedL .~ False
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      player <- getPlayer iid
      sid <- getRandom
      push
        $ chooseOne
          player
          [ SkillLabel sType [beginSkillTest sid iid (attrs.ability 1) iid sType (Fixed 3)]
          | sType <- [#combat, #agility]
          ]
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      pushAll [SetActions iid (attrs.ability 1) 0, ChooseEndTurn iid]
      pure l
    _ -> MtNgranek <$> runMessage msg attrs
