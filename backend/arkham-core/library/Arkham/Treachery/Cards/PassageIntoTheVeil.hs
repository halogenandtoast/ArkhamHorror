module Arkham.Treachery.Cards.PassageIntoTheVeil (
  passageIntoTheVeil,
  PassageIntoTheVeil (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
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
        selectAny $
          enemyIs Enemies.huntingHorror
            <> EnemyAt
              (LocationWithInvestigator $ InvestigatorWithId iid)
      push $
        beginSkillTest
          iid
          source
          (InvestigatorTarget iid)
          SkillWillpower
          (if huntingHorrorAtYourLocation then 5 else 3)
      pure t
    FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> do
          assetIds <- selectList $ AssetControlledBy (InvestigatorWithId iid) <> AllyAsset
          push $
            chooseOne
              iid
              [ Label
                  "Discard the top 5 cards of your deck"
                  [DiscardTopOfDeck iid 5 (toSource attrs) Nothing]
              , Label
                  "Take 1 direct damage and deal 1 damage to each of your Ally assets"
                  ( InvestigatorDirectDamage iid source 1 0
                      : [AssetDamage aid source 1 0 | aid <- assetIds]
                  )
              ]
          pure t
    _ -> PassageIntoTheVeil <$> runMessage msg attrs
