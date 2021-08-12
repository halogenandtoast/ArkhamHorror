module Arkham.Types.Treachery.Cards.PassageIntoTheVeil
  ( passageIntoTheVeil
  , PassageIntoTheVeil(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype PassageIntoTheVeil = PassageIntoTheVeil TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

passageIntoTheVeil :: TreacheryCard PassageIntoTheVeil
passageIntoTheVeil = treachery PassageIntoTheVeil Cards.passageIntoTheVeil

instance HasModifiersFor env PassageIntoTheVeil
instance HasActions PassageIntoTheVeil

instance TreacheryRunner env => RunMessage env PassageIntoTheVeil where
  runMessage msg t@(PassageIntoTheVeil attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      huntingHorrorAtYourLocation <- enemyAtInvestigatorLocation "02141" iid
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
        assetIds <- selectList (AssetOwnedBy You)
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
