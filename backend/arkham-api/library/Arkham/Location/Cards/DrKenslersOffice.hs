module Arkham.Location.Cards.DrKenslersOffice (drKenslersOffice) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Uses
import Arkham.Card.CardDef
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Stories

newtype DrKenslersOffice = DrKenslersOffice LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drKenslersOffice :: LocationCard DrKenslersOffice
drKenslersOffice = location DrKenslersOffice Cards.drKenslersOffice 3 (PerPlayer 2)

mirageCards :: [CardDef]
mirageCards = [Cards.memoryOfAnUnrequitedLove]

instance HasModifiersFor DrKenslersOffice where
  getModifiersFor (DrKenslersOffice a) = do
    modifySelfWhenM
      a
      ( selectAny
          $ mapOneOf
            assetIs
            [ Assets.drAmyKenslerProfessorOfBiology
            , Assets.drAmyKenslerProfessorOfBiologyResolute
            ]
          <> at_ (be a)
      )
      [ShroudModifier (-2)]
    clearedOfMirages a mirageCards

instance HasAbilities DrKenslersOffice where
  getAbilities (DrKenslersOffice a) =
    extendRevealed
      a
      [ mirage a 2 mirageCards
      , mkAbility a 1 $ forced $ SkillTestResult #after You (WhileInvestigating $ be a) #success
      ]

instance RunMessage DrKenslersOffice where
  runMessage msg l@(DrKenslersOffice attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetControlledBy iid <> AssetWithUses Secret
      chooseOneM iid do
        labeled "Take 1 horror" $ assignHorror iid (attrs.ability 1) 1
        unless (null assets) do
          labeled "Remove 1 secret from an asset you control" do
            chooseTargetM iid assets \asset -> removeTokens (attrs.ability 1) asset Secret 1
      pure l
    _ -> DrKenslersOffice <$> mirageRunner Stories.drKenslersOffice mirageCards 2 msg attrs
