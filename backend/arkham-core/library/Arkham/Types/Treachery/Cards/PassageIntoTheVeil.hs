module Arkham.Types.Treachery.Cards.PassageIntoTheVeil
  ( passageIntoTheVeil
  , PassageIntoTheVeil(..)
  )
where


import Arkham.Types.Game.Helpers
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype PassageIntoTheVeil = PassageIntoTheVeil TreacheryAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

passageIntoTheVeil :: TreacheryId -> a -> PassageIntoTheVeil
passageIntoTheVeil uuid _ = PassageIntoTheVeil $ baseAttrs uuid "02144"

instance HasModifiersFor env PassageIntoTheVeil where
  getModifiersFor = noModifiersFor

instance HasActions env PassageIntoTheVeil where
  getActions i window (PassageIntoTheVeil attrs) = getActions i window attrs

instance TreacheryRunner env => RunMessage env PassageIntoTheVeil where
  runMessage msg t@(PassageIntoTheVeil attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      huntingHorrorAtYourLocation <- enemyAtInvestigatorLocation "02141" iid
      t <$ unshiftMessage
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
        assetIds <- getSetList @AssetId iid
        t <$ unshiftMessage
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
