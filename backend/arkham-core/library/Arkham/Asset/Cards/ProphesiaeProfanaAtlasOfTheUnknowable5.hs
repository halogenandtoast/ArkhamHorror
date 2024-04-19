module Arkham.Asset.Cards.ProphesiaeProfanaAtlasOfTheUnknowable5 (
  prophesiaeProfanaAtlasOfTheUnknowable5,
  ProphesiaeProfanaAtlasOfTheUnknowable5 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Movement

newtype ProphesiaeProfanaAtlasOfTheUnknowable5 = ProphesiaeProfanaAtlasOfTheUnknowable5 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prophesiaeProfanaAtlasOfTheUnknowable5 :: AssetCard ProphesiaeProfanaAtlasOfTheUnknowable5
prophesiaeProfanaAtlasOfTheUnknowable5 = asset ProphesiaeProfanaAtlasOfTheUnknowable5 Cards.prophesiaeProfanaAtlasOfTheUnknowable5

instance HasModifiersFor ProphesiaeProfanaAtlasOfTheUnknowable5 where
  getModifiersFor (InvestigatorTarget iid) (ProphesiaeProfanaAtlasOfTheUnknowable5 attrs) | iid `controls` attrs = do
    let mlocus = maybeResult attrs.meta
    atLocus <- maybe (pure False) (\lid -> iid <=~> InvestigatorAt (LocationWithId lid)) mlocus
    pure
      $ toModifiers attrs
      $ guard (not atLocus)
      *> [SkillModifier #intellect 1, SkillModifier #agility 1, MayIgnoreAttacksOfOpportunity]
  getModifiersFor (LocationTarget lid) (ProphesiaeProfanaAtlasOfTheUnknowable5 attrs) = do
    let mlocus = maybeResult attrs.meta
    pure $ toModifiers attrs $ guard (Just lid == mlocus) *> [Locus]
  getModifiersFor _ _ = pure []

instance HasAbilities ProphesiaeProfanaAtlasOfTheUnknowable5 where
  getAbilities (ProphesiaeProfanaAtlasOfTheUnknowable5 a) =
    let mlocus = maybeResult a.meta
     in restrictedAbility a 1 ControlsThis (freeReaction $ AssetEntersPlay #after (be a))
          : [ controlledAbility
              a
              2
              (exists $ affectsOthers $ InvestigatorCanMoveTo (a.ability 2) (LocationWithId locus))
              actionAbility
            | locus <- toList mlocus
            ]

instance RunMessage ProphesiaeProfanaAtlasOfTheUnknowable5 where
  runMessage msg a@(ProphesiaeProfanaAtlasOfTheUnknowable5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      revealedLocations <- select RevealedLocation
      chooseOrRunOne
        iid
        [ targetLabel location [HandleTargetChoice iid (attrs.ability 1) (toTarget location)]
        | location <- revealedLocations
        ]
      pure a
    HandleTargetChoice _iid (isAbilitySource attrs 1 -> True) (LocationTarget lid) -> do
      pure . ProphesiaeProfanaAtlasOfTheUnknowable5 $ attrs & setMeta (Just lid)
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let locus = toResult attrs.meta
      investigators <-
        select $ affectsOthers $ InvestigatorCanMoveTo (attrs.ability 2) (LocationWithId locus)
      chooseOrRunOne
        iid
        [ targetLabel investigator [Move $ move (attrs.ability 2) investigator locus]
        | investigator <- investigators
        ]
      pure a
    _ -> ProphesiaeProfanaAtlasOfTheUnknowable5 <$> lift (runMessage msg attrs)
