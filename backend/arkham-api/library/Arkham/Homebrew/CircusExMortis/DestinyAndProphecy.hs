{- | Shared behavior for the Destiny and Prophecy story cards. Every printing of
Amalthea Weaver and De Cultus Bestiae differs only in its stats and its rider,
so the common ability shapes live here.
-}
module Arkham.Homebrew.CircusExMortis.DestinyAndProphecy where

import Arkham.Ability hiding (you)
import Arkham.Asset.Import.Lifted
import Arkham.Classes.HasGame (HasGame)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest, withSkillTestInvestigator)
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Queue (QueueT)
import Arkham.Trait (Trait (Ally))

-- * Amalthea Weaver

amaltheaWeaverAbilities :: AssetAttrs -> [Ability]
amaltheaWeaverAbilities a =
  [controlled a 1 (DuringSkillTest SkillTestAtYourLocation) $ FastAbility (exhaust a)]

{- | "The performing investigator gets +X skill value for this test, where X is
half the number of moon tokens sealed on cards at your location (rounded up)."
-}
amaltheaWeaverBoost :: ReverseQueue m => AssetAttrs -> m ()
amaltheaWeaverBoost attrs = for_ attrs.controller \you ->
  withLocationOf you \lid -> do
    n <- length <$> getSealedMoonTokensAt lid
    when (n > 0) do
      withSkillTest \sid -> withSkillTestInvestigator \performer ->
        skillTestModifier sid (attrs.ability 1) performer (AnySkillValue $ (n + 1) `div` 2)

-- | Register an "If this test is successful, ..." rider on Amalthea's ability.
amaltheaWeaverRider :: ReverseQueue m => AssetAttrs -> Message -> m ()
amaltheaWeaverRider attrs msg =
  withSkillTest \sid -> onSucceedByEffect sid AnyValue (attrs.ability 1) sid $ doStep 1 msg

{- | "You may release up to @n@ tokens sealed on a card at your location." Read
literally: any sealed token, not only ☾ (the +X clause is the ☾-only one).
-}
amaltheaWeaverRelease :: ReverseQueue m => AssetAttrs -> Int -> m ()
amaltheaWeaverRelease attrs n = for_ attrs.controller \you ->
  withLocationOf you $ chooseReleaseTokens you n <=< getSealedTokensAt

{- | "You or the performing investigator may ...": the choice belongs to
Amalthea's controller, and the Done button covers the "may".
-}
amaltheaWeaverChooseRecipient
  :: ReverseQueue m => AssetAttrs -> (InvestigatorId -> QueueT Message m ()) -> m ()
amaltheaWeaverChooseRecipient attrs f = for_ attrs.controller \you ->
  withSkillTestInvestigator \performer -> chooseUpToNM_ you 1 $ targets (nub [you, performer]) f

-- * De Cultus Bestiae

{- | The [action] that seals a moon token on the book, gated on how many tokens
it may already hold, and the [free] that releases one again.
-}
deCultusBestiaeAbilities :: Int -> AssetAttrs -> [Ability]
deCultusBestiaeAbilities sealLimit a =
  [ doesNotProvokeAttacksOfOpportunity
      $ controlled a 1 (thisExists a underLimit <> exists moonToken) actionAbility
  , controlled a 2 (thisExists a $ AssetWithSealedChaosTokens 1 AnyChaosToken) $ FastAbility Free
  ]
 where
  underLimit = not_ $ AssetWithSealedChaosTokens (sealLimit + 1) AnyChaosToken

-- | "Release a token sealed on De Cultus Bestiae."
deCultusBestiaeRelease :: ReverseQueue m => InvestigatorId -> AssetAttrs -> m ()
deCultusBestiaeRelease iid attrs =
  chooseReleaseToken iid =<< select (SealedOnAsset (be attrs) AnyChaosToken)

-- | Investigators and Ally assets at @iid@'s location that @source@ could heal.
healableCardsAt
  :: (HasGame m, Sourceable source) => source -> InvestigatorId -> m ([InvestigatorId], [AssetId])
healableCardsAt source iid = do
  investigators <-
    select
      $ oneOf [HealableInvestigator (toSource source) k (colocatedWith iid) | k <- [#damage, #horror]]
  assets <-
    select
      $ oneOf
        [ HealableAsset (toSource source) k (at_ (locationWithInvestigator iid) <> withTrait Ally)
        | k <- [#damage, #horror]
        ]
  pure (investigators, assets)

-- | "Heal 2 damage or 2 horror (or any combination thereof)" from one card.
chooseHealTwo
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => source -> InvestigatorId -> target -> Bool -> Bool -> m ()
chooseHealTwo source you target canDamage canHorror = chooseOneM you $ withI18n do
  when canDamage $ countVar 2 $ labeled' "healDamage" $ healDamage target source 2
  when canHorror $ countVar 2 $ labeled' "healHorror" $ healHorror target source 2
  when (canDamage && canHorror)
    $ withVars ["damage" .= (1 :: Int), "horror" .= (1 :: Int)]
    $ labeled' "healDamageAndHorror" do
      healDamage target source 1
      healHorror target source 1
