module Arkham.Treachery.Cards.PassageIntoTheVeil (passageIntoTheVeil, PassageIntoTheVeil (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype PassageIntoTheVeil = PassageIntoTheVeil TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

passageIntoTheVeil :: TreacheryCard PassageIntoTheVeil
passageIntoTheVeil = treachery PassageIntoTheVeil Cards.passageIntoTheVeil

instance RunMessage PassageIntoTheVeil where
  runMessage msg t@(PassageIntoTheVeil attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      huntingHorrorAtYourLocation <-
        selectAny $ enemyIs Enemies.huntingHorror <> at_ (locationWithInvestigator iid)
      push
        $ revelationSkillTest
          iid
          source
          #willpower
          (Fixed $ if huntingHorrorAtYourLocation then 5 else 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assetIds <- select $ assetControlledBy iid <> #ally
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label "Discard the top 5 cards of your deck" [DiscardTopOfDeck iid 5 (toSource attrs) Nothing]
          , Label
              "Take 1 direct damage and deal 1 damage to each of your Ally assets"
              $ InvestigatorDirectDamage iid (toSource attrs) 1 0
              : [Msg.AssetDamage aid (toSource attrs) 1 0 | aid <- assetIds]
          ]
      pure t
    _ -> PassageIntoTheVeil <$> runMessage msg attrs
