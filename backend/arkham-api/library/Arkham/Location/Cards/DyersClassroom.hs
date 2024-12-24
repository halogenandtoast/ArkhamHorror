module Arkham.Location.Cards.DyersClassroom (dyersClassroom) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card.CardDef
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Stories

newtype DyersClassroom = DyersClassroom LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dyersClassroom :: LocationCard DyersClassroom
dyersClassroom = location DyersClassroom Cards.dyersClassroom 5 (PerPlayer 2)

mirageCards :: [CardDef]
mirageCards = [Cards.memoryOfARegretfulVoyage]

instance HasModifiersFor DyersClassroom where
  getModifiersFor (DyersClassroom a) = do
    modifySelfWhenM
      a
      ( selectAny
          $ mapOneOf
            assetIs
            [ Assets.professorWilliamDyerProfessorOfGeology
            , Assets.professorWilliamDyerProfessorOfGeologyResolute
            ]
          <> at_ (be a)
      )
      [ShroudModifier (-2)]
    clearedOfMirages a mirageCards

instance HasAbilities DyersClassroom where
  getAbilities (DyersClassroom a) =
    extendRevealed
      a
      [ mirage a 2 mirageCards
      , restricted
          a
          1
          ( Here
              <> oneOf
                [ exists (HealableInvestigator (a.ability 1) #horror (at_ $ be a))
                , exists (HealableAsset (a.ability 1) #horror (at_ $ be a))
                ]
          )
          $ actionAbilityWithCost
          $ ShuffleTopOfScenarioDeckIntoYourDeck 1 TekeliliDeck
      ]

instance RunMessage DyersClassroom where
  runMessage msg l@(DyersClassroom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ HealableInvestigator (attrs.ability 1) #horror (at_ $ be attrs)
      assets <- select $ HealableAsset (attrs.ability 1) #horror (at_ $ be attrs)
      chooseOneM iid do
        targets investigators \investigator -> healHorror investigator (attrs.ability 1) 1
        targets assets \asset -> healHorror asset (attrs.ability 1) 1
      pure l
    _ -> DyersClassroom <$> mirageRunner Stories.dyersClassroom mirageCards 2 msg attrs
