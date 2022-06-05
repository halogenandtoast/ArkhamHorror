module Arkham.Treachery.Cards.PassageIntoTheVeil
  ( passageIntoTheVeil
  , PassageIntoTheVeil(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Treachery.Attrs
import qualified Arkham.Treachery.Cards as Cards

newtype PassageIntoTheVeil = PassageIntoTheVeil TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor m, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

passageIntoTheVeil :: TreacheryCard PassageIntoTheVeil
passageIntoTheVeil = treachery PassageIntoTheVeil Cards.passageIntoTheVeil

instance RunMessage PassageIntoTheVeil where
  runMessage msg t@(PassageIntoTheVeil attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      huntingHorrorAtYourLocation <-
        selectAny $ enemyIs Enemies.huntingHorror <> EnemyAtLocation
          (LocationWithInvestigator $ InvestigatorWithId iid)
      t <$ push
        (BeginSkillTest
          iid
          source
          (InvestigatorTarget iid)
          Nothing
          SkillWillpower
          (if huntingHorrorAtYourLocation then 5 else 3)
        )
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        assetIds <- selectList (AssetControlledBy You)
        t <$ push
          (chooseOne
            iid
            [ Label
              "Discard the top 5 cards of your deck"
              [DiscardTopOfDeck iid 5 Nothing]
            , Label
              "Take 1 direct damage and deal 1 damage to each of your Ally assets"
              (InvestigatorDirectDamage iid source 1 0
              : [ AssetDamage aid source 1 0 | aid <- assetIds ]
              )
            ]
          )
    _ -> PassageIntoTheVeil <$> runMessage msg attrs
